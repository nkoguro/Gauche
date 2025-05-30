;;;
;;; compile-5.scm - The compiler: Pass 5
;;;
;;;   Copyright (c) 2004-2025  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;===============================================================
;; Pass 5.  Code generation
;;

;; This pass passes down a runtime environment, renv.  It is
;; a nested list of lvars, and used to generate LREF/LSET instructions.
;;
;; The context, ctx, is either one of the following symbols.
;;
;;   normal/bottom : the FORM is evaluated in the context that the
;;            stack has no pending arguments (i.e. a continuation
;;            frame is just pushed).
;;   normal/top : the FORM is evaluated, while there are pending
;;            arguments in the stack top.  Such premature argument frame
;;            should be protected if VM calls something that may
;;            capture the continuation.
;;   stmt/bottom : Like normal/bottom, but the result of FORM won't
;;            be used.
;;   stmt/top : Like normal/top, but the result of FORM won't be used.
;;   tail   : FORM is evaluated in the tail context.  It is always
;;            bottom.
;;
;; Each IForm node handler generates the code by side-effects.  Besides
;; the code generation, each handler returns the maximum stack depth.
;;
;; The return value of pass5 entry is a <compiled-code>.
;; The pass5 main entry may be called recursively from $LAMBDA node;
;; the CLOSURE instruction takes <compiled-code> of the body of the lambda
;; as an operand.

;; predicates
(define-inline (normal-context? ctx)
  (or (eq? ctx 'normal/bottom) (eq? ctx 'normal/top)))
(define-inline (stmt-context? ctx)
  (or (eq? ctx 'stmt/bottom) (eq? ctx 'stmt/top)))
(define-inline (tail-context? ctx)
  (eq? ctx 'tail))
(define-inline (bottom-context? ctx)
  (or (eq? ctx 'normal/bottom) (eq? ctx 'stmt/bottom) (eq? ctx 'tail)))
(define-inline (top-context? ctx)
  (or (eq? ctx 'normal/top) (eq? ctx 'stmt/top)))

;; context switch
(define-inline (normal-context prev-ctx)
  (if (bottom-context? prev-ctx) 'normal/bottom 'normal/top))
(define-inline (stmt-context prev-ctx)
  (if (bottom-context? prev-ctx) 'stmt/bottom 'stmt/top))
(define-inline (tail-context prev-ctx) 'tail)

;; whether we can use LREF/LSET
(define-inline (small-env? depth offset)
  (and (<= 0 depth  (- (ash 1 VM_INSN_ARG0_BITS) 1))
       (<= 0 offset (- (ash 1 VM_INSN_ARG1_BITS) 1))))

;; For brevity, these macro assumes variable 'target'.
(define-macro (cont-frame-size)
  '(ctarget-cont-frame-size target))
(define-macro (env-header-size)
  '(ctarget-env-header-size target))


;; Dispatch pass5 handler.
;; *pass5-dispatch-table* is defined below, after all handlers are defined.
(define-inline (pass5/rec iform target renv ctx)
  ((vector-ref *pass5-dispatch-table* (vector-ref iform 0))
   iform target renv ctx))

;;
;; Pass 5 main entry.  Returns <compiled-code>
;;
(define (pass5 iform target initial-renv ctx)
  (let1 maxstack (pass5/rec iform target initial-renv ctx)
    (rlet1 ccb (ctarget-ccb target)
      (compiled-code-emit-RET! ccb)
      (compiled-code-finish-builder ccb maxstack))))

;;
;; Pass 5 intermediate tree handlers
;;

(define (pass5/$DEFINE iform target renv ctx)
  (let ([d (pass5/rec ($define-expr iform) target renv 'normal/bottom)]
        [f (cond [(memq 'const ($define-flags iform)) SCM_BINDING_CONST]
                 [(memq 'inlinable ($define-flags iform)) SCM_BINDING_INLINABLE]
                 [else 0])])
    (compiled-code-emit1oi! (ctarget-ccb target) DEFINE f
                            ($define-id iform) ($*-src iform))
    d))

(define (pass5/$LREF iform target renv ctx)
  (receive (depth offset) (renv-lookup renv ($lref-lvar iform))
    (if (small-env? depth offset)
      (compiled-code-emit2i! (ctarget-ccb target) LREF depth offset
                             (lvar-name ($lref-lvar iform)))
      (compiled-code-emit1oi! (ctarget-ccb target) XLREF offset depth
                              (lvar-name ($lref-lvar iform))))
    (unless (lvar-immutable? ($lref-lvar iform))
      (compiled-code-emit0! (ctarget-ccb target) UNBOX))
    0))

(define (pass5/$LSET iform target renv ctx)
  (receive (depth offset) (renv-lookup renv ($lset-lvar iform))
    (rlet1 d (pass5/rec ($lset-expr iform) target renv (normal-context ctx))
      (if (small-env? depth offset)
        (compiled-code-emit2i! (ctarget-ccb target) LSET depth offset
                               (lvar-name ($lset-lvar iform)))
        (compiled-code-emit1oi! (ctarget-ccb target) XLSET offset depth
                                (lvar-name ($lset-lvar iform)))))))

(define (pass5/$GREF iform target renv ctx)
  (let1 id ($gref-id iform)
    (compiled-code-emit0oi! (ctarget-ccb target) GREF id id)
    0))

(define (pass5/$GSET iform target renv ctx)
  (rlet1 d (pass5/rec ($gset-expr iform) target renv (normal-context ctx))
    (let1 id ($gset-id iform)
      (compiled-code-emit0oi! (ctarget-ccb target) GSET id id))))

(define (pass5/$CONST iform target renv ctx)
  ;; if the context is stmt-context, value won't be used so we drop it.
  (unless (stmt-context? ctx)
    (compiled-code-emit0o! (ctarget-ccb target) CONST ($const-value iform)))
  0)

;; Branch peephole optimization
;;   We have variations of conditional branch instructions for typical
;;   cases.  In this handler we select an appropriate instructions.
;;
;;   Sometimes we want to inverse the test, swapping then and else,
;;   if we can strip extra NOT operation.  Note that it is only possible
;;   if the result of test isn't used directly (that is, neither then nor
;;   else clause is ($IT)), thus we treat such a case specially.
(define (pass5/$IF iform target renv ctx)
  (cond
   [(and (not ($it? ($if-then iform)))
         (not ($it? ($if-else iform)))
         (has-tag? ($if-test iform) $ASM)
         (eqv? (car ($asm-insn ($if-test iform))) NOT))
    (pass5/$IF ($if ($*-src iform)
                    (car ($asm-args ($if-test iform)))
                    ($if-else iform)
                    ($if-then iform))
               target renv ctx)]
   [else
    (pass5/branch-core iform target renv ctx)]))

(define (pass5/branch-core iform target renv ctx)
  (let1 test ($if-test iform)
    ;; Select an appropriate branch instruction
    (cond
     [(has-tag? test $ASM)
      (let ([code (car ($asm-insn test))]; ASM code
            [args ($asm-args test)])
        (cond
         [(eqv? code NULLP)
          (pass5/if-final iform (car args) BNNULL 0 0
                          ($*-src test) target renv ctx)]
         [(eqv? code EQ)
          (pass5/if-eq iform (car args) (cadr args)
                       ($*-src test) target renv ctx)]
         [(eqv? code EQV)
          (pass5/if-eqv iform (car args) (cadr args)
                        ($*-src test) target renv ctx)]
         [(eqv? code NUMEQ2)
          (pass5/if-numeq iform (car args) (cadr args)
                          ($*-src test) target renv ctx)]
         [(eqv? code NUMLE2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNLE ($*-src test) target renv ctx)]
         [(eqv? code NUMLT2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNLT ($*-src test) target renv ctx)]
         [(eqv? code NUMGE2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNGE ($*-src test) target renv ctx)]
         [(eqv? code NUMGT2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNGT ($*-src test) target renv ctx)]
         [else
          (pass5/if-final iform test BF 0 0 ($*-src iform) target renv ctx)]
         ))]
     [(has-tag? test $EQ?)
      (pass5/if-eq iform ($*-arg0 test) ($*-arg1 test)
                   ($*-src iform) target renv ctx)]
     [(has-tag? test $EQV?)
      (pass5/if-eqv iform ($*-arg0 test) ($*-arg1 test)
                    ($*-src iform) target renv ctx)]
     [($const? test)   ; this may occur as a result of macro expansion
      (pass5/rec (if ($const-value test)
                   (if ($it? ($if-then iform)) test ($if-then iform))
                   (if ($it? ($if-else iform)) test ($if-else iform)))
                 target renv ctx)]
     [else
      (pass5/if-final iform test BF 0 0 ($*-src iform) target renv ctx)]
     )))

;;
(define (pass5/if-eq iform x y info target renv ctx)
  (cond
   [($const? x) (pass5/if-final iform y BNEQC ($const-value x)
                                0 info target renv ctx)]
   [($const? y) (pass5/if-final iform x BNEQC ($const-value y)
                                0 info target renv ctx)]
   [else
    (let1 depth (imax (pass5/rec x target renv (normal-context ctx)) 1)
      (compiled-code-emit-PUSH! (ctarget-ccb target))
      (pass5/if-final iform #f BNEQ 0
                      (imax (pass5/rec y target renv 'normal/top) depth)
                      info target renv ctx))]))

(define (pass5/if-eqv iform x y info target renv ctx)
  (cond
   [($const? x) (pass5/if-final iform y BNEQVC ($const-value x)
                                0 info target renv ctx)]
   [($const? y) (pass5/if-final iform x BNEQVC ($const-value y)
                                0 info target renv ctx)]
   [else
    (let1 depth (imax (pass5/rec x target renv (normal-context ctx)) 1)
      (compiled-code-emit-PUSH! (ctarget-ccb target))
      (pass5/if-final iform #f BNEQV 0
                      (imax (pass5/rec y target renv 'normal/top) depth)
                      info target renv ctx))]))

(define (pass5/if-numeq iform x y info target renv ctx)
  (or (and ($const? x)
           (integer-fits-insn-arg? ($const-value x))
           (pass5/if-final iform y BNUMNEI ($const-value x)
                           0
                           info target renv ctx))
      (and ($const? y)
           (integer-fits-insn-arg? ($const-value y))
           (pass5/if-final iform x BNUMNEI ($const-value y)
                           0
                           info target renv ctx))
      (and ($lref? x)
           (lvar-immutable? ($lref-lvar x))
           (receive (depth offset) (renv-lookup renv ($lref-lvar x))
             (and (small-env? depth offset)
                  (pass5/if-final iform #f LREF-VAL0-BNUMNE
                                  (pass5/if-numcmp-lrefarg x renv)
                                  (pass5/rec y target renv (normal-context ctx))
                                  info target renv ctx))))
      (and ($lref? y)
           (lvar-immutable? ($lref-lvar y))
           (receive (depth offset) (renv-lookup renv ($lref-lvar y))
             (and (small-env? depth offset)
                  (pass5/if-final iform #f LREF-VAL0-BNUMNE
                                  (pass5/if-numcmp-lrefarg y renv)
                                  (pass5/rec x target renv (normal-context ctx))
                                  info target renv ctx))))
      (let1 depth (imax (pass5/rec x target renv (normal-context ctx)) 1)
        (compiled-code-emit-PUSH! (ctarget-ccb target))
        (pass5/if-final iform #f BNUMNE 0
                        (imax (pass5/rec y target renv 'normal/top) depth)
                        info target renv ctx))))

(define (pass5/if-numcmp iform x y insn info target renv ctx)
  (define .fwd. `((,BNLT . ,LREF-VAL0-BNLT) (,BNLE . ,LREF-VAL0-BNLE)
                  (,BNGT . ,LREF-VAL0-BNGT) (,BNGE . ,LREF-VAL0-BNGE)))
  (define .rev. `((,BNLT . ,LREF-VAL0-BNGT) (,BNLE . ,LREF-VAL0-BNGE)
                  (,BNGT . ,LREF-VAL0-BNLT) (,BNGE . ,LREF-VAL0-BNLE)))
  (or (and ($lref? x)
           (lvar-immutable? ($lref-lvar x))
           (receive (depth offset) (renv-lookup renv ($lref-lvar x))
             (and (small-env? depth offset)
                  (pass5/if-final iform #f (cdr (assv insn .fwd.))
                                  (pass5/if-numcmp-lrefarg x renv)
                                  (pass5/rec y target renv (normal-context ctx))
                                  info target renv ctx))))
      (and ($lref? y)
           (lvar-immutable? ($lref-lvar y))
           (receive (depth offset) (renv-lookup renv ($lref-lvar y))
             (and (small-env? depth offset)
                  (pass5/if-final iform #f (cdr (assv insn .rev.))
                                  (pass5/if-numcmp-lrefarg y renv)
                                  (pass5/rec x target renv (normal-context ctx))
                                  info target renv ctx))))
      (let1 depth (imax (pass5/rec x target renv (normal-context ctx)) 1)
        (compiled-code-emit-PUSH! (ctarget-ccb target))
        (pass5/if-final iform #f insn 0
                        (imax (pass5/rec y target renv 'normal/top) depth)
                        info target renv ctx))))

;; helper fn
(define (pass5/if-numcmp-lrefarg lref renv)
  (receive (dep off) (renv-lookup renv ($lref-lvar lref))
    (+ (ash off 10) dep)))


;; pass5/if-final: Final stage of emitting branch instruction.
;;
;; Optimization
;;   - tail context
;;      - if insn is (BF)
;;        - then part is ($IT)  => use RT
;;        - else part is ($IT)  => use RF
;;        - then part is jump back to labeled closure
;;          => use BT, and use the destination of the label. (*)
;;      - otherwise, place RET after then clause
;;   - otherwise
;;      - else part is ($IT)  => we can omit a jump after then clause
;;      - otherwise, merge the control after this node.
;;
;;  (*) This converts the left asm output to the right one.  This type of
;;      code appears as the result of the first pattern of $if optimization
;;      in pass3.
;;
;;        L0:                   L0:
;;            ...                   ...
;;            BF L1                 BT L0
;;            JUMP L0               xxx
;;        L1: xxx                   ...
;;            ...
;;
;; We have many variations of branch instrucitons, and the combination
;; of arguments reflects them.
;;
;;   iform - original IForm of this if expression.
;;   test - the iform for the test expression.  the result of this expression
;;          would trigger the conditional branch.  This can be #f when we use
;;          operate-and-branch instructions such as BNLT.
;;   code - an instruciton code.
;;   arg0/opr - If the instruction is one of those that take an extra operand
;;          (like BNEQC), this is the operand.  Otherwise, this is the ARG0
;;          of the instruction.
;;   depth - calculated maximum stack depth at this point.
;;   info  - source info
;;   ccb   - code buffer
;;   renv  - runtime env
;;   ctx   - compile context

(define-constant .branch-insn-extra-operand.
  `(,BNEQC ,BNEQVC))

(define (pass5/if-final iform test code arg0/opr depth info target renv ctx)
  (let ([depth (if test
                 (imax (pass5/rec test target renv (normal-context ctx)) depth)
                 depth)]
        [then-form ($if-then iform)]
        [else-form ($if-else iform)])
    (cond
     [(tail-context? ctx)
      (or (and (eqv? code BF)
               (cond
                [($it? then-form)
                 (compiled-code-emit0i! (ctarget-ccb target) RT info)
                 (imax (pass5/rec else-form target renv ctx) depth)]
                [($it? else-form)
                 (compiled-code-emit0i! (ctarget-ccb target) RF info)
                 (imax (pass5/rec then-form target renv ctx) depth)]
                [(and (has-tag? then-form $LABEL)
                      ($label-label then-form))
                 => (^[label]
                      (compiled-code-emit0o! (ctarget-ccb target) BT label)
                      (imax (pass5/rec else-form target renv ctx) depth))]
                [else #f]))
          (let* ([ccb (ctarget-ccb target)]
                 [elselabel (compiled-code-new-label ccb)])
            (if (memv code .branch-insn-extra-operand.)
              (compiled-code-emit0oi! ccb code (list arg0/opr elselabel) info)
              (compiled-code-emit1oi! ccb code arg0/opr elselabel info))
            (set! depth (imax (pass5/rec then-form target renv ctx) depth))
            (compiled-code-emit-RET! ccb)
            (compiled-code-set-label! ccb elselabel)
            (imax (pass5/rec else-form target renv ctx) depth)))]
     [else
      (let* ([ccb (ctarget-ccb target)]
             [elselabel  (compiled-code-new-label ccb)]
             [mergelabel (compiled-code-new-label ccb)])
        (if (memv code .branch-insn-extra-operand.)
          (compiled-code-emit0oi! ccb code (list arg0/opr elselabel) info)
          (compiled-code-emit1oi! ccb code arg0/opr elselabel info))
        (set! depth (imax (pass5/rec then-form target renv ctx) depth))
        (unless ($it? else-form)
          (compiled-code-emit0o! ccb JUMP mergelabel))
        (compiled-code-set-label! ccb elselabel)
        (unless ($it? else-form)
          (set! depth (imax (pass5/rec else-form target renv ctx) depth)))
        (compiled-code-set-label! ccb mergelabel)
        depth)])))

(define (pass5/$IT iform target renv ctx) 0)

;; $LET stack estimate
;;   normal let: Each init clause is evaluated while preceding results
;;     of inits are on the stack.  Pass5/prepare-args returns the maximum
;;     stack depth from the initial position of the stack (i.e. it considers
;;     accumulating values).  After all inits are evaluated, we complete
;;     the env frame and run the body.
;;
;;   letrec: We create the env frame before evaluating inits, so the max
;;     stack is: total env frame size + max of stack depth consumed by
;;     one of inits or the body.
;;
(define (pass5/$LET iform target renv ctx)
  (when (and (has-tag? ($let-body iform) $LET)
             (eq? ($let-type iform) 'let))
    (pass5/flatten-let*! iform))
  (let ([info ($*-src iform)]
        [lvars ($let-lvars iform)]
        [inits ($let-inits iform)]
        [body  ($let-body iform)]
        [merge-label (if (bottom-context? ctx)
                       #f
                       (compiled-code-new-label (ctarget-ccb target)))])
    (when (> (length lvars) (- (ash 1 VM_INSN_ARG_BITS) 1))
      (errorf "[internal] Local frame size (~a) exceeded compiler limitation (~a)."
              (length lvars) (- (ash 1 VM_INSN_ARG_BITS) 1)))
    (let ([nlocals (length lvars)]
          [ccb (ctarget-ccb target)])
      (case ($let-type iform)
        [(let)
         (cond
          [(bottom-context? ctx)
           (let1 dinit (pass5/prepare-args inits target renv ctx)
             (compiled-code-emit1i! ccb LOCAL-ENV nlocals info)
             (pass5/box-mutable-lvars lvars ccb)
             (let1 dbody (pass5/rec body target (cons lvars renv) ctx)
               (unless (tail-context? ctx)
                 (compiled-code-emit0! ccb POP-LOCAL-ENV))
               (imax dinit (+ dbody (env-header-size) nlocals))))]
          [else
           (compiled-code-emit1oi! ccb PRE-CALL nlocals merge-label info)
           (let1 dinit (pass5/prepare-args inits target renv ctx)
             (compiled-code-emit1i! ccb LOCAL-ENV nlocals info)
             (pass5/box-mutable-lvars lvars ccb)
             (let1 dbody (pass5/rec body target (cons lvars renv) 'tail)
               (compiled-code-emit-RET! ccb)
               (compiled-code-set-label! ccb merge-label)
               (imax dinit
                     (+ dbody (cont-frame-size) (env-header-size) nlocals))))])]
        [(rec rec*)
         (receive (closures others)
             (partition-letrec-inits inits target (cons lvars renv) 0 '() '())
           (cond
            [(bottom-context? ctx)
             (compiled-code-emit1oi! ccb LOCAL-ENV-CLOSURES nlocals
                                     closures info)
             (emit-letrec-boxers ccb lvars nlocals)
             (let* ([dinit (emit-letrec-inits others lvars nlocals target
                                              (cons lvars renv) 0)]
                    [dbody (pass5/rec body target (cons lvars renv) ctx)])
               (unless (tail-context? ctx)
                 (compiled-code-emit0! ccb POP-LOCAL-ENV))
               (+ (env-header-size) nlocals (imax dinit dbody)))]
            [else
             (compiled-code-emit1oi! ccb PRE-CALL nlocals merge-label info)
             (compiled-code-emit1oi! ccb LOCAL-ENV-CLOSURES nlocals
                                     closures info)
             (emit-letrec-boxers ccb lvars nlocals)
             (let* ([dinit (emit-letrec-inits others lvars nlocals target
                                              (cons lvars renv) 0)]
                    [dbody (pass5/rec body target (cons lvars renv) 'tail)])
               (compiled-code-emit-RET! ccb)
               (compiled-code-set-label! ccb merge-label)
               (+ (cont-frame-size) (env-header-size) nlocals
                  (imax dinit dbody)))]))]
        [else
         (error "[internal error]: pass5/$LET got unknown let type:"
                ($let-type iform))]
        ))))

;; Nested let can be flattened if the initexpr of inner local vars
;; does not depend on the values of the local vars of the current frame,
;; and they don't possibly capture environments/continuations.
;; $const and $lref to outer frames are certainly safe.
;; $gref also won't capture frames, but we have to be careful if we ever
;; provide a mechanism to restart from undefined variable error.
(define (pass5/flatten-let*! iform)
  (let loop ([lvars (reverse ($let-lvars iform))]
             [inits (reverse ($let-inits iform))]
             [node ($let-body iform)])
    (let1 ins ($let-inits node)
      (cond [(everyc safe-lvar-initval-for-flatten? ins lvars)
             (let ([lvars (reverse ($let-lvars node) lvars)]
                   [inits (reverse ins inits)])
               (if (and (has-tag? ($let-body node) $LET)
                        (eq? ($let-type node) 'let))
                 (loop lvars inits ($let-body node))
                 (begin ($let-lvars-set! iform (reverse! lvars))
                        ($let-inits-set! iform (reverse! inits))
                        ($let-body-set! iform ($let-body node)))))]
            [(eq? node ($let-body iform))] ; we didn't do anything
            [else ($let-lvars-set! iform (reverse! lvars))
                  ($let-inits-set! iform (reverse! inits))
                  ($let-body-set! iform node)]))))

(define (safe-lvar-initval-for-flatten? init existing-lvars)
  (and (or ($const? init)
           (and ($lref? init)
                (not (memq ($lref-lvar init) existing-lvars))))))

;; classify init values into closures/constants and non-closure expressions.
;; returns two lists: a list of init values (closures or #under values)
;; corresponding to lvar list, and non-closure init iforms, each
;; paired with an lvar count.
(define (partition-letrec-inits inits target renv cnt closures others)
  (if (null? inits)
    (values (reverse closures) (reverse others))
    (let1 init (car inits)
      (cond
       [(has-tag? init $LAMBDA)
        (partition-letrec-inits (cdr inits) target renv (+ cnt 1)
                                (cons (pass5/lambda init target renv) closures)
                                others)]
       [($const? init)
        (partition-letrec-inits (cdr inits) target renv (+ cnt 1)
                                (cons ($const-value init) closures)
                                others)]
       [else
        (partition-letrec-inits (cdr inits) target renv (+ cnt 1)
                                (cons (undefined) closures)
                                (acons cnt init others))]))))

;; box set!-able slots in the ENV at stack top.  used in letrec and
;; receive frame setup.
(define (emit-letrec-boxers ccb lvars nlocals)
  (let loop ([lvars lvars] [cnt nlocals])
    (unless (null? lvars)
      (unless (lvar-immutable? (car lvars))
        (compiled-code-emit1! ccb BOX cnt))
      (loop (cdr lvars) (- cnt 1)))))

;; emit LSET or ENV-SET insn to initialize lvars that aren't closures or
;; constants.  init-alist is the second value partition-letrec-inits
;; returned, which is a list of (<lvar-count> <init-iform>).
(define (emit-letrec-inits init-alist lvars nlocals target renv depth)
  (if (null? init-alist)
    depth
    (let* ([off&expr (car init-alist)]
           [d (pass5/rec (cdr off&expr) target renv 'normal/bottom)]
           [lvar (list-ref lvars (car off&expr))]
           [ccb (ctarget-ccb target)])
      (if (lvar-immutable? lvar)
        (compiled-code-emit1! ccb ENV-SET (- nlocals 1 (car off&expr)))
        (compiled-code-emit2! ccb LSET 0 (- nlocals 1 (car off&expr))))
      (emit-letrec-inits (cdr init-alist) lvars nlocals target renv
                         (imax depth d)))))

(define (pass5/$RECEIVE iform target renv ctx)
  (let ([nargs  ($receive-reqargs iform)]
        [optarg ($receive-optarg iform)]
        [lvars  ($receive-lvars iform)]
        [expr   ($receive-expr iform)]
        [body   ($receive-body iform)]
        [ccb    (ctarget-ccb target)])
    (cond
     [(bottom-context? ctx)
      (let1 dinit (pass5/rec expr target renv (normal-context ctx))
        (compiled-code-emit2i! ccb TAIL-RECEIVE nargs optarg ($*-src iform))
        (emit-letrec-boxers ccb lvars (length lvars))
        (let1 dbody (pass5/rec body target (cons lvars renv) ctx)
          (unless (tail-context? ctx)
            (compiled-code-emit0! ccb POP-LOCAL-ENV))
          (imax dinit (+ nargs optarg (env-header-size) dbody))))]
     [else
      (let ([merge-label (compiled-code-new-label ccb)]
            [dinit (pass5/rec expr target renv (normal-context ctx))])
        (compiled-code-emit2oi! ccb RECEIVE nargs optarg
                                merge-label ($*-src iform))
        (emit-letrec-boxers ccb lvars (length lvars))
        (let1 dbody (pass5/rec body target (cons lvars renv) 'tail)
          (compiled-code-emit-RET! ccb)
          (compiled-code-set-label! ccb merge-label)
          (imax dinit (+ nargs optarg (cont-frame-size) (env-header-size) dbody))))]
     )))

(define (pass5/$LAMBDA iform target renv ctx)
  (let ([code (pass5/lambda iform target renv)]
        [info ($*-src iform)])
    (compiled-code-emit0oi! (ctarget-ccb target) CLOSURE code info))
  0)

(define (pass5/lambda iform target renv)
  (let* ([ccb (make-compiled-code-builder ($lambda-reqargs iform)
                                          ($lambda-optarg iform)
                                          ($lambda-name iform)
                                          (ctarget-ccb target) ; parent
                                          ($lambda-inliner iform))]
         [ntarget (make-child-compile-target ccb target)])
    (compiled-code-attach-source-info! ccb ($lambda-src iform) ($lambda-type iform))

    ;; If any of procedure parameters are set!, we should box it
    ;; upon entering the procedure.
    (let loop ([lvs ($lambda-lvars iform)]
               [k (length ($lambda-lvars iform))])
      (unless (null? lvs)
        (unless (lvar-immutable? (car lvs))
          (compiled-code-emit1i! ccb BOX k (lvar-name (car lvs))))
        (loop (cdr lvs) (- k 1))))
    ;; Save list of unused arguments in the attributes of (car signature-info).
    (let1 uargs (filter-map (^[lv] (and (zero? (lvar-ref-count lv))
                                        (lvar-name lv)))
                            ($lambda-lvars iform))
      (unless (null? uargs)
        (pair-attribute-set! (car (slot-ref ccb 'signature-info))
                             'unused-args uargs)))
    ;; Run pass5 on body
    (pass5 ($lambda-body iform)
           ntarget
           (if (null? ($lambda-lvars iform))
             renv
             (cons ($lambda-lvars iform) renv))
           'tail)))

(define (pass5/$CLAMBDA iform target renv ctx)
  (define (reqargs-min-max argcounts)
    (let loop ([counts argcounts] [mi #f] [mx 0])
      (match counts
        [() (values mi mx)]
        [((req . _) . rest) (loop rest (if mi (min mi req) req) (max mx req))])))
  (let*-values ([(cs) ($clambda-closures iform)]
                [(ccb) (ctarget-ccb target)]
                [(minarg maxarg) (reqargs-min-max ($clambda-argcounts iform))]
                [merger (if (bottom-context? ctx)
                          #f
                          (compiled-code-new-label ccb))])
    (when merger
      (compiled-code-emit1oi! ccb PRE-CALL 4 merger ($*-src iform)))
    (compiled-code-emit0o! ccb CONST minarg)
    (compiled-code-emit-PUSH! ccb)
    (compiled-code-emit0o! ccb CONST maxarg)
    (compiled-code-emit-PUSH! ccb)
    (compiled-code-emit0o! ccb CONST #f)
    (let loop ([xs cs] [depth 0] [cnt 0])
      (cond
       [(null? xs)
        (compiled-code-emit1! ccb LIST (length cs))
        (compiled-code-emit-PUSH! ccb)
        (compiled-code-emit0o! ccb CONST ($clambda-name iform))
        (compiled-code-emit-PUSH! ccb)
        (compiled-code-emit0oi! ccb GREF make-case-lambda. make-case-lambda.)
        (compiled-code-emit1i! ccb CALL 4 ($*-src iform))
        (when merger
          (compiled-code-set-label! ccb merger))
        (+ depth 3)]
       [else
        (compiled-code-emit-PUSH! ccb)
        (let1 d (pass5/rec (car xs) target renv 'normal/top)
          (loop (cdr xs) (imax depth (+ d cnt 1)) (+ cnt 1)))]))))

(define (pass5/$LABEL iform target renv ctx)
  (let ([label ($label-label iform)]
        [ccb (ctarget-ccb target)])
    ;; NB: $LABEL node in the PROC position of $CALL node is handled by $CALL.
    (cond
     [label (compiled-code-emit0oi! ccb JUMP label ($*-src iform))
            0]
     [else  (compiled-code-set-label! ccb (pass5/ensure-label ccb iform))
            (pass5/rec ($label-body iform) target renv ctx)])))

(define (pass5/$SEQ iform target renv ctx)
  (let1 exprs ($seq-body iform)
    (cond
     [(null? exprs) 0]
     [(null? (cdr exprs)) (pass5/rec (car exprs) target renv ctx)]
     [else
      (let loop ([exprs exprs] [depth 0])
        (if (null? (cdr exprs))
          (imax (pass5/rec (car exprs) target renv ctx) depth)
          (loop (cdr exprs)
                (imax (pass5/rec (car exprs) target renv (stmt-context ctx))
                      depth))))])))

;; $CALL.
;;  There are several variations in $CALL node.  Each variation may also
;;  have tail-call version and non-tail-call version.
;;
;;  1. Local call: a $CALL node that has 'local' flag is a call to known
;;     local procedure.  Its arguments are already adjusted to match the
;;     signature of the procedure.   PROC slot contains an LREF node that
;;     points to the local procedure.
;;
;;  2. Embedded call: a $CALL node that has 'embed' flag is a control
;;     transfer to an inlined local procedure, whose entry point may be
;;     called from more than one place (Cf. an inlined procedure that is
;;     called only once becomes $LET node, so we don't need to consider it).
;;     Its arguments are already adjusted to match the signature of the
;;     procedure.  Its PROC slot contains the embedded $LAMBDA node, whose
;;     body is $LABEL node.
;;     The generated code is almost the same as $LET node, except that a
;;     label is placed just after LOCAL-ENV.
;;
;;     We also record the RENV in this node, which is later used by
;;     jump call node to determine the number of environment frames the
;;     LOCAL-ENV-JUMP should discard.  (Here we assume an embed node always
;;     goes through pass5 before related jump nodes.)
;;
;;  3. Jump call: a $CALL node that has 'jump' flag is a control transfer
;;     to an inlined local procedure, and whose body is embedded in somewhere
;;     else (by an 'embedded call' node).   The PROC slot contains the embed
;;     $CALL node.  We emit LOCAL-ENV-JUMP instruction for this type of node.
;;
;;  4. Head-heavy call: a $CALL node without any flag, and all the
;;     arguments are simple expressions (e.g. const or lref), but the
;;     operator expression has $LET.  The normal calling sequence evaluates
;;     the operator expression after pushing arguments.  That causes the
;;     $LET be evaluated in 'top' context, which requires pushing
;;     extra continuation.  If all the arguments are simple, we can evaluate
;;     the operator expression first, and keeping it in VAL0 while pushing
;;     the arguments.
;;     Notably, a named let expression tends to become a head-heavy call,
;;     so it is worth to treat it specially.
;;     Note that this head-heavy call optimization relies on the arguments
;;     to use combined instructions such as CONST-PUSH or LREF-PUSH.  If
;;     the instruction combination is turned off, we can't use this since
;;     VAL0 is overwritten by arguments.
;;
;;  5. Other call node generates the standard calling sequence.
;;

;; stack depth of $CALL nodes:
;;  - if nargs >= 1, we need (# of args) + (env header) slots
;;  - if generic call, +2 for possible object-apply hack and next-method.
;;  - if non-tail call, + (cont-frame-size).

(define (pass5/$CALL iform target renv ctx)
  (case ($call-flag iform)
    [(local) (pass5/local-call iform target renv ctx)]
    [(embed) (pass5/embed-call iform target renv ctx)]
    [(jump)  (pass5/jump-call  iform target renv ctx)]
    [else
     (if (and (bottom-context? ctx)
              (has-tag? ($call-proc iform) $LET)
              (all-args-simple? ($call-args iform))
              (not (vm-compiler-flag-is-set? SCM_COMPILE_NOCOMBINE)))
       (pass5/head-heavy-call iform target renv ctx)
       (pass5/normal-call iform target renv ctx))]))

;; Local call
;;   PROC is always $LREF.
(define (pass5/local-call iform target renv ctx)
  (let* ([args ($call-args iform)]
         [nargs (length args)]
         [ccb (ctarget-ccb target)])
    (if (tail-context? ctx)
      (let1 dinit (pass5/prepare-args args target renv ctx)
        (pass5/rec ($call-proc iform) target renv 'normal/top)
        (compiled-code-emit1i! ccb LOCAL-ENV-TAIL-CALL nargs ($*-src iform))
        (if (= nargs 0) 0 (imax dinit (+ nargs (env-header-size)))))
      (let1 merge-label (compiled-code-new-label ccb)
        (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
        (let1 dinit (pass5/prepare-args args target renv ctx)
          (pass5/rec ($call-proc iform) target renv 'normal/top)
          (compiled-code-emit1i! ccb LOCAL-ENV-CALL nargs ($*-src iform))
          (compiled-code-set-label! ccb merge-label)
          (if (= nargs 0)
            (cont-frame-size)
            (imax dinit (+ nargs (env-header-size) (cont-frame-size)))))))))

;; Embedded call
;;   $call-proc has $lambda node.  We inline its body.
;;   We also record the RENV to the current node, so that the jump calls
;;   to the inlined body can adjust env frame properly.
(define (pass5/embed-call iform target renv ctx)
  (let* ([proc ($call-proc iform)]
         [args ($call-args iform)]
         [nargs (length args)]
         [label ($lambda-body proc)]
         [lvars ($lambda-lvars proc)]
         [newenv (if (= nargs 0) renv (cons lvars renv))]
         [ccb (ctarget-ccb target)]
         [merge-label (compiled-code-new-label ccb)])
    ($call-renv-set! iform (reverse renv))
    (unless (tail-context? ctx)
      (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform)))
    (let1 dinit (if (> nargs 0)
                  (rlet1 d (pass5/prepare-args args target renv ctx)
                    (compiled-code-emit1i! ccb LOCAL-ENV nargs ($*-src iform))
                    (pass5/box-mutable-lvars lvars ccb))
                  0)
      (compiled-code-set-label! ccb (pass5/ensure-label ccb label))
      (let1 dbody (pass5/rec ($label-body label) target newenv 'tail)
        (compiled-code-emit-RET! ccb)
        (compiled-code-set-label! ccb merge-label)
        (if (= nargs 0)
          (+ (cont-frame-size) dbody)
          (imax dinit (+ nargs (env-header-size) (cont-frame-size) dbody)))))
    ))

;; Jump call
;;   $call-proc has a $call[embed] node, whose proc slot has $lambda
;;   node, whose proc slot has $label node.
;; NB: we're not sure whether we'll have non-tail jump call yet.
(define (pass5/jump-call iform target renv ctx)
  (let ([args ($call-args iform)]
        [embed-node ($call-proc iform)]
        [ccb (ctarget-ccb target)])
    (let ([nargs (length args)]
          [label ($lambda-body ($call-proc embed-node))]
          [lvars ($lambda-lvars ($call-proc embed-node))]
          [renv-diff (list-remove-prefix ($call-renv embed-node)
                                         (reverse renv))])
      (unless renv-diff
        (errorf "[internal error] $call[jump] appeared out of context of related $call[embed] (~s vs ~s)"
                ($call-renv embed-node) renv))
      (if (tail-context? ctx)
        (let1 dinit (pass5/prepare-args args target renv ctx)
          (pass5/emit-local-env-jump ccb lvars (length renv-diff)
                                     (pass5/ensure-label ccb label)
                                     ($*-src iform))
          (if (= nargs 0) 0 (imax dinit (+ nargs (env-header-size)))))
        (let1 merge-label (compiled-code-new-label ccb)
          (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
          (let1 dinit (pass5/prepare-args args target renv ctx)
            (pass5/emit-local-env-jump ccb lvars (length renv-diff)
                                       (pass5/ensure-label ccb label)
                                       ($*-src iform))
            (compiled-code-set-label! ccb merge-label)
            (if (= nargs 0)
              (cont-frame-size)
              (imax dinit (+ nargs (env-header-size) (cont-frame-size))))))
        ))))

(define (pass5/emit-local-env-jump ccb lvars env-depth label src)
  (let loop ([lvs lvars])
    (cond [(null? lvs)  ; no need of boxing.
           (compiled-code-emit1oi! ccb LOCAL-ENV-JUMP env-depth label src)]
          [(not (lvar-immutable? (car lvs))) ; need boxing
           (compiled-code-emit1i! ccb LOCAL-ENV-SHIFT env-depth src)
           (pass5/box-mutable-lvars lvars ccb)
           (compiled-code-emit0oi! ccb JUMP label src)]
          [else
           (loop (cdr lvs))])))

;; Head-heavy call
(define (pass5/head-heavy-call iform target renv ctx)
  (let* ([args ($call-args iform)]
         [nargs (length args)]
         [ccb (ctarget-ccb target)])
    (if (tail-context? ctx)
      (let* ([dproc (pass5/rec ($call-proc iform)
                               target renv (normal-context ctx))]
             [dinit (pass5/prepare-args args target renv 'normal/top)])
        (compiled-code-emit1i! ccb TAIL-CALL nargs ($*-src iform))
        (imax dinit (+ nargs dproc (env-header-size))))
      (let1 merge-label (compiled-code-new-label ccb)
        (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
        (let* ([dproc (pass5/rec ($call-proc iform)
                                 target renv (normal-context ctx))]
               [dinit (pass5/prepare-args args target renv 'normal/top)])
          (compiled-code-emit1i! ccb CALL nargs ($*-src iform))
          (compiled-code-set-label! ccb merge-label)
          (+ (cont-frame-size) (imax dinit (+ nargs dproc (env-header-size))))))
      )))

;; Normal call
(define (pass5/normal-call iform target renv ctx)
  (let* ([args ($call-args iform)]
         [nargs (length args)]
         [ccb (ctarget-ccb target)])
    (if (tail-context? ctx)
      (let* ([dinit (pass5/prepare-args args target renv ctx)]
             [dproc (pass5/rec ($call-proc iform) target renv 'normal/top)])
        (compiled-code-emit1i! ccb TAIL-CALL nargs ($*-src iform))
        (imax dinit (+ nargs dproc (env-header-size))))
      (let1 merge-label (compiled-code-new-label ccb)
        (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
        (let* ([dinit (pass5/prepare-args args target renv ctx)]
               [dproc (pass5/rec ($call-proc iform) target renv 'normal/top)])
          (compiled-code-emit1i! ccb CALL nargs ($*-src iform))
          (compiled-code-set-label! ccb merge-label)
          (+ (cont-frame-size) (imax dinit (+ nargs dproc (env-header-size))))))
      )))

(define (all-args-simple? args)
  (cond [(null? args) #t]
        [(memv (iform-tag (car args)) `(,$LREF ,$CONST))
         (all-args-simple? (cdr args))]
        [else #f]))

;; Returns a part of lis whose head is removed.  If HEAD is not a prefix
;; of LIS, returns #f.
(define (list-remove-prefix head lis)
  (let loop ((head head) (lis lis))
    (cond [(null? head) lis]
          [(null? lis) #f]
          [(eq? (car head) (car lis)) (loop (cdr head) (cdr lis))]
          [else #f])))

(define (pass5/ensure-label ccb label-node)
  (or ($label-label label-node)
      (rlet1 lab (compiled-code-new-label ccb)
        ($label-label-set! label-node lab))))

;; $DYNENV
(define (pass5/$DYNENV iform target renv ctx)
  (let* ([ccb (ctarget-ccb target)]
         [merge-label (and (not (tail-context? ctx))
                           (compiled-code-new-label ccb))]
         [dcont (if (tail-context? ctx)
                  0
                  (begin
                    (compiled-code-emit1oi! ccb PRE-CALL 0 merge-label
                                            ($*-src iform))
                    (cont-frame-size)))]
         [dkey (pass5/rec ($dynenv-key iform) target renv 'normal/bottom)]
         [flag (if (memq 'push ($dynenv-flags iform)) 1 0)])
    (compiled-code-emit-PUSH! ccb)
    (let* ([dval (pass5/rec ($dynenv-value iform) target renv 'normal/top)]
           [dkv  (imax dkey (+ dval 1))])
      (compiled-code-emit1i! ccb TAIL-EXTEND-DENV flag ($*-src iform))
      (let1 dbody (pass5/rec ($dynenv-body iform) target renv 'tail)
        (unless (tail-context? ctx)
          (compiled-code-emit-RET! ccb)
          (compiled-code-set-label! ccb merge-label))
        (+ dcont (imax dkv dbody))))))

;; $ASMs.  For some instructions, we may pick more specialized one
;; depending on its arguments.

(define (pass5/$ASM iform target renv ctx)
  (let ([info ($*-src iform)]
        [insn ($asm-insn iform)]
        [args ($asm-args iform)])
    (case/unquote
     (car insn)
     [(EQ)
      (pass5/asm-eq  info (car args) (cadr args) target renv ctx)]
     [(EQV)
      (pass5/asm-eqv info (car args) (cadr args) target renv ctx)]
     [(NUMEQ2)
      (pass5/asm-numeq2 info (car args) (cadr args) target renv ctx)]
     [(NUMLT2 NUMLE2 NUMGT2 NUMGE2)
      (pass5/asm-numcmp info (car insn) (car args) (cadr args) target renv ctx)]
     [(NUMADD2)
      (pass5/asm-numadd2 info (car args) (cadr args) target renv ctx)]
     [(NUMSUB2)
      (pass5/asm-numsub2 info (car args) (cadr args) target renv ctx)]
     [(NUMMUL2)
      (pass5/asm-nummul2 info (car args) (cadr args) target renv ctx)]
     [(NUMDIV2)
      (pass5/asm-numdiv2 info (car args) (cadr args) target renv ctx)]
     [(LOGAND LOGIOR LOGXOR)
      (pass5/asm-bitwise info (car insn) (car args) (cadr args) target renv ctx)]
     [(VEC-REF)
      (pass5/asm-vec-ref info (car args) (cadr args) target renv ctx)]
     [(VEC-SET)
      (pass5/asm-vec-set info (car args) (cadr args) (caddr args) target renv ctx)]
     [(SLOT-REF)
      (pass5/asm-slot-ref info (car args) (cadr args) target renv ctx)]
     [(SLOT-SET)
      (pass5/asm-slot-set info (car args) (cadr args) (caddr args) target renv ctx)]
     [(TAIL-APPLY)
      ;; TAIL-APPLY pushes restarg onto stack, thus +1.
      (if (tail-context? ctx)
        (+ 1 (pass5/asm-generic target insn args info renv))
        (let* ([ccb (ctarget-ccb target)]
               [merge-label (compiled-code-new-label ccb)])
          (compiled-code-emit0oi! ccb PRE-CALL merge-label info)
          (let1 d (pass5/asm-generic target insn args info renv)
            (compiled-code-set-label! ccb merge-label)
            (+ (cont-frame-size) d 1))))]
     [else
      (pass5/asm-generic target insn args info renv)])))

(define (pass5/asm-generic target insn args info renv)
  ;; general case
  (let1 ccb (ctarget-ccb target)
    (case (length args)
      [(0) (pass5/emit-asm! ccb insn info) 0]
      [(1) (rlet1 d (pass5/rec (car args) target renv 'normal/top)
             (pass5/emit-asm! ccb insn info))]
      [(2) (let1 d0 (pass5/rec (car args) target renv 'normal/top)
             (compiled-code-emit-PUSH! ccb)
             (let1 d1 (pass5/rec (cadr args) target renv 'normal/top)
               (pass5/emit-asm! ccb insn info)
               (imax d0 (+ d1 1))))]
      [else
       (let loop ([args args] [depth 0] [cnt 0])
         (cond [(null? (cdr args))
                (let1 d (pass5/rec (car args) target renv 'normal/top)
                  (pass5/emit-asm! ccb insn info)
                  (imax depth (+ cnt d)))]
               [else
                (let1 d (pass5/rec (car args) target renv 'normal/top)
                  (compiled-code-emit-PUSH! ccb)
                  (loop (cdr args) (imax depth (+ d cnt)) (+ cnt 1)))]))]
      )))

(define (pass5/emit-asm! ccb insn info)
  (match insn
    [(code)           (compiled-code-emit0i! ccb code info)]
    [(code arg0)      (compiled-code-emit1i! ccb code arg0 info)]
    [(code arg0 arg1) (compiled-code-emit2i! ccb code arg0 arg1 info)]))

;; Utility macros.  Assumes target, renv and ctx are visible.

(define-macro (pass5/builtin-twoargs info code param arg0 arg1)
  (let ([d0 (gensym)]
        [d1 (gensym)])
    `(let1 ,d0 (pass5/rec ,arg0 target renv (normal-context ctx))
       (compiled-code-emit-PUSH! (ctarget-ccb target))
       (let1 ,d1 (pass5/rec ,arg1 target renv 'normal/top)
         ,(if (equal? param 0)
            `(compiled-code-emit0i! (ctarget-ccb target) ,code ,info)
            `(compiled-code-emit1i! (ctarget-ccb target) ,code ,param ,info))
         (imax ,d0 (+ ,d1 1))))))

(define-macro (pass5/builtin-onearg info code param arg0)
  (let1 d (gensym)
    `(rlet1 ,d (pass5/rec ,arg0 target renv (normal-context ctx))
       ,(if (equal? param 0)
          `(compiled-code-emit0i! (ctarget-ccb target) ,code ,info)
          `(compiled-code-emit1i! (ctarget-ccb target) ,code ,param ,info)))))

(define-macro (pass5/builtin-onearg+operand info code param operand arg0)
  (let1 d (gensym)
    `(rlet1 ,d (pass5/rec ,arg0 target renv (normal-context ctx))
       ,(if (equal? param 0)
          `(compiled-code-emit0oi! (ctarget-ccb target) ,code ,operand ,info)
          `(compiled-code-emit1oi! (ctarget-ccb target) ,code ,param ,operand ,info)))))

(define-macro (pass5/builtin-nargs info code args)
  `(%pass5/builtin-nargs target ,info ,code ,args (ctarget-ccb target) renv))

(define (%pass5/builtin-nargs target info code args ccb renv)
  (if (null? args)
    (begin (compiled-code-emit0i! ccb code info) 0)
    (let loop ([as args] [depth 0] [cnt 0])
      (cond [(null? (cdr as))
             (let1 d (pass5/rec (car as) target renv 'normal/top)
               (compiled-code-emit1i! ccb code (length args) info)
               (imax (+ d cnt) depth))]
            [else
             (let1 d (pass5/rec (car as) target renv 'normal/top)
               (compiled-code-emit-PUSH! ccb)
               (loop (cdr as) (imax (+ d cnt) depth) (+ cnt 1)))]))))

(define (pass5/$CONS iform target renv ctx)
  (pass5/builtin-twoargs ($*-src iform)
                         CONS 0 ($*-arg0 iform) ($*-arg1 iform)))

(define (pass5/$APPEND iform target renv ctx)
  (pass5/builtin-twoargs ($*-src iform) APPEND 2 ($*-arg0 iform) ($*-arg1 iform)))

(define (pass5/$LIST iform target renv ctx)
  (pass5/builtin-nargs ($*-src iform) LIST ($*-args iform)))

(define (pass5/$LIST* iform target renv ctx)
  (pass5/builtin-nargs ($*-src iform) LIST-STAR ($*-args iform)))

(define (pass5/$VECTOR iform target renv ctx)
  (pass5/builtin-nargs ($*-src iform) VEC ($*-args iform)))

(define (pass5/$LIST->VECTOR iform target renv ctx)
  (pass5/builtin-onearg ($*-src iform) LIST2VEC 0 ($*-arg0 iform)))

(define (pass5/$MEMV iform target renv ctx)
  (pass5/builtin-twoargs ($*-src iform) MEMV 0 ($*-arg0 iform) ($*-arg1 iform)))

(define (pass5/$EQ? iform target renv ctx)
  (pass5/asm-eq ($*-src iform) ($*-arg0 iform) ($*-arg1 iform) target renv ctx))

(define (pass5/$EQV? iform target renv ctx)
  (pass5/asm-eqv ($*-src iform) ($*-arg0 iform) ($*-arg1 iform) target renv ctx))

;; handlers to emit specialized instruction when applicable

(define (pass5/asm-eq info x y target renv ctx)
  (pass5/builtin-twoargs info EQ 0 x y))

(define (pass5/asm-eqv info x y target renv ctx)
  (pass5/builtin-twoargs info EQV 0 x y))

(define (pass5/asm-numeq2 info x y target renv ctx)
  (pass5/builtin-twoargs info NUMEQ2 0 x y))

(define (pass5/asm-numcmp info code x y target renv ctx)
  (pass5/builtin-twoargs info code 0 x y))

(define (pass5/asm-numadd2 info x y target renv ctx)
  (or (and ($const? x)
           (integer-fits-insn-arg? ($const-value x))
           (pass5/builtin-onearg info NUMADDI ($const-value x) y))
      (and ($const? y)
           (integer-fits-insn-arg? ($const-value y))
           (pass5/builtin-onearg info NUMADDI ($const-value y) x))
      (and ($lref? y)
           (lvar-immutable? ($lref-lvar y))
           (receive (depth offset) (renv-lookup renv ($lref-lvar y))
             (and (small-env? depth offset)
                  (pass5/builtin-onearg info LREF-VAL0-NUMADD2
                                        (+ (ash offset 10) depth) x))))
      (and ($lref? x)
           (lvar-immutable? ($lref-lvar x))
           (receive (depth offset) (renv-lookup renv ($lref-lvar x))
             (and (small-env? depth offset)
                  (pass5/builtin-onearg info LREF-VAL0-NUMADD2
                                        (+ (ash offset 10) depth) y))))
      (pass5/builtin-twoargs info NUMADD2 0 x y)))

(define (pass5/asm-numsub2 info x y target renv ctx)
  (or (and ($const? x)
           (integer-fits-insn-arg? ($const-value x))
           (pass5/builtin-onearg info NUMSUBI ($const-value x) y))
      (and ($const? y)
           (integer-fits-insn-arg? ($const-value y))
           (pass5/builtin-onearg info NUMADDI (- ($const-value y)) x))
      (pass5/builtin-twoargs info NUMSUB2 0 x y)))

(define (pass5/asm-nummul2 info x y target renv ctx)
  (pass5/builtin-twoargs info NUMMUL2 0 x y))

(define (pass5/asm-numdiv2 info x y target renv ctx)
  (pass5/builtin-twoargs info NUMDIV2 0 x y))

;; if one of arg is constant, it's always x.  see builtin-inline-bitwise below.
(define (pass5/asm-bitwise info insn x y target renv ctx)
  (define lookup `((,LOGAND . ,LOGANDC)
                   (,LOGIOR . ,LOGIORC)
                   (,LOGXOR . ,LOGXORC)))
  (if ($const? x)
    (pass5/builtin-onearg+operand info (assv-ref lookup insn)
                                  0 ($const-value x) y)
    (pass5/builtin-twoargs info insn 0 x y)))

(define (pass5/asm-vec-ref info vec k target renv ctx)
  (cond [(and ($const? k)
              (unsigned-integer-fits-insn-arg? ($const-value k)))
         (pass5/builtin-onearg info VEC-REFI ($const-value k) vec)]
        [else
         (pass5/builtin-twoargs info VEC-REF 0 vec k)]))

(define (pass5/asm-vec-set info vec k obj target renv ctx)
  (cond [(and ($const? k)
              (unsigned-integer-fits-insn-arg? ($const-value k)))
         (pass5/builtin-twoargs info VEC-SETI ($const-value k) vec obj)]
        [else
         (let1 d0 (pass5/rec vec target renv (normal-context ctx))
           (compiled-code-emit-PUSH! (ctarget-ccb target))
           (let1 d1 (pass5/rec k target renv 'normal/top)
             (compiled-code-emit-PUSH! (ctarget-ccb target))
             (let1 d2 (pass5/rec obj target renv 'normal/top)
               (compiled-code-emit0i! (ctarget-ccb target) VEC-SET info)
               (imax d0 (+ d1 1) (+ d2 2)))))]))

(define (pass5/asm-slot-ref info obj slot target renv ctx)
  (cond [($const? slot)
         (rlet1 d (pass5/rec obj target renv (normal-context ctx))
           (compiled-code-emit0oi! (ctarget-ccb target)
                                   SLOT-REFC ($const-value slot) info))]
        [else
         (pass5/builtin-twoargs info SLOT-REF 0 obj slot)]))

(define (pass5/asm-slot-set info obj slot val target renv ctx)
  (cond [($const? slot)
         (let1 d0 (pass5/rec obj target renv (normal-context ctx))
           (compiled-code-emit-PUSH! (ctarget-ccb target))
           (let1 d1 (pass5/rec val target renv 'normal/top)
             (compiled-code-emit0oi! (ctarget-ccb target)
                                     SLOT-SETC ($const-value slot) info)
             (imax d0 (+ d1 1))))]
        [else
         (let1 d0 (pass5/rec obj target renv (normal-context ctx))
           (compiled-code-emit-PUSH! (ctarget-ccb target))
           (let1 d1 (pass5/rec slot target renv 'normal/top)
             (compiled-code-emit-PUSH! (ctarget-ccb target))
             (let1 d2 (pass5/rec val target renv 'normal/top)
               (compiled-code-emit0i! (ctarget-ccb target) SLOT-SET info)
               (imax d0 (+ d1 1) (+ d2 2)))))]))

;; Dispatch table.
(define *pass5-dispatch-table* (generate-dispatch-table pass5))

;; Returns depth and offset of local variable reference.
;;   renv-lookup : [[Lvar]], Lvar -> Int, Int
;;
(inline-stub
 (define-cproc renv-lookup (renv lvar)
   (let* ([depth::int 0])
     (dolist [fp renv]
       (let* ([count::int 1])
         (dolist [lp fp]
           (when (SCM_EQ lp lvar)
             (return (values (SCM_MAKE_INT depth)
                             (SCM_MAKE_INT (- (Scm_Length fp) count)))))
           (pre++ count)))
       (pre++ depth)))
   (Scm_Error "[internal error] stray local variable: %S" lvar)
   (return SCM_UNDEFINED)) ; dummy
 )

;; Emit code to evaluate expressions in args and push its result
;; into the stack one by one.  Returns the maximum depth of the stack.
;; lvars is #f for regular call sequence, or a list of lvars of the same
;; length of args for $LET or local calls.
(define (pass5/prepare-args args target renv ctx)
  (if (null? args)
    0
    (let1 d (pass5/rec (car args) target renv (normal-context ctx))
      (compiled-code-emit-PUSH! (ctarget-ccb target))
      ;; NB: We check termination condition here.  This routine is called
      ;; lots of times, and (length args) is usually small (<=2 covers almost
      ;; half of the cases, and <=3 covers over 80%).  Check termination
      ;; condition before entering loop saves extra calculation of loop
      ;; arguments, and it is not negligible in this case.
      (if (null? (cdr args))
        d
        (let loop ([args  (cdr args)]
                   [depth (+ d 1)]
                   [cnt  1])
          (let1 d (pass5/rec (car args) target renv 'normal/top)
            (compiled-code-emit-PUSH! (ctarget-ccb target))
            (if (null? (cdr args))
              (imax depth d)
              (loop (cdr args) (imax depth (+ d cnt 1)) (+ cnt 1)))))))))

;; In case of $LET
(define (pass5/box-mutable-lvars lvars ccb)
  (let1 envsize (length lvars)
    (let loop ([lvars lvars]
               [k 0])
      (unless (null? lvars)
        (unless (lvar-immutable? (car lvars))
          (compiled-code-emit1i! ccb BOX (- envsize k) (lvar-name (car lvars))))
        (loop (cdr lvars) (+ k 1))))))
