;;;
;;; Generate native code vector for FFI
;;;

;; TODO: Eventually we should be able to omit construction of patcher list
;; as it's the task `asm-instantiate` should handle.  However, it would
;; require filling a bytevector with binary represenation of appropriate
;; native types.  Native type infrastructure should be able to handle it,
;; but we can't depend on it here, as gen-native.scm needs to run with
;; 0.9.15.   So we put off that modification after 0.9.16 release.
;;
;; The idea is that we let lang.asm.x86_64 handle patching instead of
;; Scm_VMCallNative.

(use scheme.list)
(use util.match)
(use gauche.uvector)
(use file.util)
(use lang.asm.x86_64)

;;;
;;; Common Prologue
;;;

(define (emit-header port)
  (display ";; libnative.scm supplemental code.\n" port)
  (display ";; Generated automatically by gen-native.scm.  DO NOT EDIT.\n" port)
  (display "\n" port))

;;; Helper: look up the byte offset of a placeholder keyword in a patches list.
(define (patch-offset kw patches)
  (let1 p (assq kw patches)
    (if p (cadr p)
        (error "patch not found:" kw))))

;;; Helper: look up the byte offset, returning #f if not present.
(define (patch-offset/maybe kw patches)
  (let1 p (assq kw patches)
    (and p (cadr p))))

;;; For SYSV AMD64 calling convention: Section 3.2 of
;;; http://refspecs.linux-foundation.org/elf/x86_64-abi-0.95.pdf

(define (gen-stub-amd64 port)
  ;; When all args can be on registers.
  ;; The code tail jumps to the target, so the target's return directly
  ;; returns to the caller of call-amd64.  Thus we can reuse the same
  ;; codepad area without worrying the recursive calls overwrite active
  ;; code.
  (define reg-tmpl
    (asm-template
     '(func:       (.dataq :func)
       farg0:      (.dataq :farg0)
       farg1:      (.dataq :farg1)
       farg2:      (.dataq :farg2)
       farg3:      (.dataq :farg3)
       farg4:      (.dataq :farg4)
       farg5:      (.dataq :farg5)
       farg6:      (.dataq :farg6)
       farg7:      (.dataq :farg7)
       entry6f7:   ((movs_ :farg7-variant) (farg7:) %xmm7)
       entry6f6:   ((movs_ :farg6-variant) (farg6:) %xmm6)
       entry6f5:   ((movs_ :farg5-variant) (farg5:) %xmm5)
       entry6f4:   ((movs_ :farg4-variant) (farg4:) %xmm4)
       entry6f3:   ((movs_ :farg3-variant) (farg3:) %xmm3)
       entry6f2:   ((movs_ :farg2-variant) (farg2:) %xmm2)
       entry6f1:   ((movs_ :farg1-variant) (farg1:) %xmm1)
       entry6f0:   ((movs_ :farg0-variant) (farg0:) %xmm0)
       entry6:     (movq (imm64 :iarg5) %r9)
       entry5:     (movq (imm64 :iarg4) %r8)
       entry4:     (movq (imm64 :iarg3) %rcx)
       entry3:     (movq (imm64 :iarg2) %rdx)
       entry2:     (movq (imm64 :iarg1) %rsi)
       entry1:     (movq (imm64 :iarg0) %rdi)
       entry0:     (movb (imm8 :num-fargs) %al)
                   (jmp (func:))
       end:)))

  ;; Spill case.
  (define spill-tmpl
    (asm-template
     '(func:       (.dataq :func)
       farg0:      (.dataq :farg0)
       farg1:      (.dataq :farg1)
       farg2:      (.dataq :farg2)
       farg3:      (.dataq :farg3)
       farg4:      (.dataq :farg4)
       farg5:      (.dataq :farg5)
       farg6:      (.dataq :farg6)
       farg7:      (.dataq :farg7)
       entry6f7:   ((movs_ :farg7-variant) (farg7:) %xmm7)
       entry6f6:   ((movs_ :farg6-variant) (farg6:) %xmm6)
       entry6f5:   ((movs_ :farg5-variant) (farg5:) %xmm5)
       entry6f4:   ((movs_ :farg4-variant) (farg4:) %xmm4)
       entry6f3:   ((movs_ :farg3-variant) (farg3:) %xmm3)
       entry6f2:   ((movs_ :farg2-variant) (farg2:) %xmm2)
       entry6f1:   ((movs_ :farg1-variant) (farg1:) %xmm1)
       entry6f0:   ((movs_ :farg0-variant) (farg0:) %xmm0)
       entry6:     (movq (imm64 :iarg5) %r9)
       entry5:     (movq (imm64 :iarg4) %r8)
       entry4:     (movq (imm64 :iarg3) %rcx)
       entry3:     (movq (imm64 :iarg2) %rdx)
       init:       (movq (imm32 :init-spill-size) %rax)
                   (leaq (spill: %rip) %rsi)
       loop:       (movq (%rsi) %rdi)
                   (push %rdi)
                   (addq 8 %rsi)
                   (subq 8 %rax)
                   (jnz loop:)
       entry2:     (movq (imm64 :iarg1) %rsi)
       entry1:     (movq (imm64 :iarg0) %rdi)
       entry0:     (movb (imm8 :num-fargs) %al)
                   (call (func:))
       epilogue:   (addq (imm32 :epilogue-spill-size) %rsp)
                   (ret)
                   (.align 8)
       spill:)))

  (define reg-labels  (asm-template-labels  reg-tmpl))
  (define reg-patches (asm-template-patches reg-tmpl))
  (define spill-labels  (asm-template-labels  spill-tmpl))
  (define spill-patches (asm-template-patches spill-tmpl))

  ;; entry-offsets :: labels -> list-of-fixnum
  ;;   entry0:..entry6: for int args, then entry6f0:..entry6f7: for float args.
  (define (entry-offsets labels)
    (map (cut assq-ref labels <>)
         '(entry0: entry1: entry2: entry3: entry4: entry5: entry6:
                   entry6f0: entry6f1: entry6f2: entry6f3:
                   entry6f4: entry6f5: entry6f6: entry6f7:)))
  ;; iarg-offsets :: patches -> list-of-fixnum
  ;;   Byte offsets of the imm64 fields for iarg0..iarg5.
  (define (iarg-offsets patches)
    (map (cut patch-offset <> patches)
         '(:iarg0 :iarg1 :iarg2 :iarg3 :iarg4 :iarg5)))
  ;; farg-offsets :: patches -> list-of-fixnum
  ;;   Byte offsets of the .dataq slots for farg0..farg7.
  (define (farg-offsets patches)
    (map (cut patch-offset <> patches)
         '(:farg0 :farg1 :farg2 :farg3 :farg4 :farg5 :farg6 :farg7)))
  ;; movsX-offsets :: patches -> list-of-fixnum-or-#f
  ;;   Byte offsets of the movs_ prefix bytes for farg0-variant..farg7-variant.
  (define (movsX-offsets patches)
    (map (cut patch-offset/maybe <> patches)
         '(:farg0-variant :farg1-variant :farg2-variant :farg3-variant
           :farg4-variant :farg5-variant :farg6-variant :farg7-variant)))
  (define (end-addr labels) (assq-ref labels 'end:))

  (display ";; Register-only calling" port)
  (display ";; label    offset\n" port)
  (dolist [p reg-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (pprint `(define *amd64-call-reg-code* ,(asm-template-bytes reg-tmpl))
          :port port
          ;; TRANSIENT: :radix -> :radix-prefix after the new release
          :controls (make-write-controls :pretty #t :width 75
                                         :base 16 :radix #t))

  (display ";; Spill-to-stack case" port)
  (display ";; label    offset\n" port)
  (dolist [p spill-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (pprint `(define *amd64-call-spill-code* ,(asm-template-bytes spill-tmpl))
          :port port
          ;; TRANSIENT: :radix -> :radix-prefix after the new release
          :controls (make-write-controls :pretty #t :width 75
                                         :base 16 :radix #t))

  (pprint
   `(define (%iarg-type? t)
      (or (eq? t <top>)
          (subtype? t <integer>)
          (eq? t <c-string>)
          (is-a? t <c-pointer>)
          (is-a? t <c-array>)
          (is-a? t <c-function>)))
   :port port)

  (pprint
   `(define (%farg-type? t)
      (or (eq? t <double>)
          (eq? t <float>)))
   :port port)

  (pprint '(define-constant movss-prefix #xf3)  ; movss opcode prefix (vs movsd's #xf2)
          :port port)

  ;; (call-amd64 <dlptr> args rettype)
  ;;  args : ((type value) ...)
  ;; NB: In the final form, we won't expose this function to the user; it's
  ;; too error-prone.  You can wreck havoc just by passing a wrong type.
  ;; Instead, we'll require the user to parse the C function declaration
  ;; and we automatically extract the type info.
  (pprint
   `(define call-amd64
      (^[ptr args rettype]
        (let* ([num-iargs (count (^p (%iarg-type? (car p))) args)]
               [num-fargs (count (^p (%farg-type? (car p))) args)]
               [num-spills (+ (max 0 (- num-iargs 6))
                              (max 0 (- num-fargs 8)))])
          (if (zero? num-spills)
            (call-amd64-regs  ptr args num-iargs num-fargs rettype)
            (call-amd64-spill ptr args
                              (min num-iargs 6)
                              (min num-fargs 8)
                              num-spills rettype)))))
   :port port)
  (pprint
   `(define call-amd64-regs
      (let ((%%call-native (module-binding-ref 'gauche.bootstrap '%%call-native))
            (entry-offsets ',(entry-offsets reg-labels))
            (iarg-offsets  ',(iarg-offsets reg-patches))
            (farg-offsets  ',(farg-offsets reg-patches))
            (movsX-offsets ',(movsX-offsets reg-patches))
            (func-offset   ,(patch-offset :func reg-patches))
            (nfargs-offset ,(patch-offset :num-fargs reg-patches)))
        (^[ptr args num-iargs num-fargs rettype]
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry (~ entry-offsets effective-nargs)]
                 [patcher
                  (let loop ([args args] [icount 0] [fcount 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ icount 1) fcount
                                 (cons `(,(~ iarg-offsets icount)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           (loop (cdr args) icount (+ fcount 1)
                                 (cond-list
                                  [#t `(,(~ farg-offsets fcount) ,@(car args))]
                                  [(eq? (caar args) <float>)
                                   `(,(~ movsX-offsets fcount) ,<uint8> ,movss-prefix)]
                                  [#t @ r]))]
                          [else (error "bad arg entry:" (car args))]))])
            (%%call-native entry 0
                           *amd64-call-reg-code*
                           entry
                           ,(end-addr reg-labels)
                           entry
                           (list* `(,func-offset ,<void*> ,ptr)
                                  `(,nfargs-offset ,<uint8> ,num-fargs)
                                  patcher)
                           rettype)))))
   :port port)
  (pprint
   `(define call-amd64-spill
      (let ((%%call-native (module-binding-ref 'gauche.bootstrap '%%call-native))
            (entry-offsets     ',(entry-offsets spill-labels))
            (iarg-offsets      ',(iarg-offsets spill-patches))
            (farg-offsets      ',(farg-offsets spill-patches))
            (movsX-offsets     ',(movsX-offsets spill-patches))
            (func-offset       ,(patch-offset :func spill-patches))
            (nfargs-offset     ,(patch-offset :num-fargs spill-patches))
            (spill-offset      (^n (+ ,(assq-ref spill-labels 'spill:)
                                      (* n 8))))
            (init-spill-offset    ,(patch-offset :init-spill-size spill-patches))
            (epilogue-spill-offset ,(patch-offset :epilogue-spill-size spill-patches)))
        (^[ptr args num-iargs num-fargs num-spills rettype]
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry (~ entry-offsets effective-nargs)]
                 [patcher
                  (let loop ([args args] [icount 0] [fcount 0] [scount 0]
                             [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (if (< icount 6)
                             (loop (cdr args) (+ icount 1) fcount scount
                                   (cons `(,(~ iarg-offsets icount)
                                           ,@(car args))
                                         r))
                             (loop (cdr args) (+ icount 1) fcount
                                   (+ scount 1)
                                   (cons `(,(spill-offset (- num-spills scount 1))
                                           ,@(car args))
                                         r)))]
                          [(%farg-type? (caar args))
                           (if (< fcount 8)
                             (loop (cdr args) icount (+ fcount 1) scount
                                   (cond-list
                                    [#t `(,(~ farg-offsets fcount) ,@(car args))]
                                    [(eq? (caar args) <float>)
                                     `(,(~ movsX-offsets fcount) ,<uint8> ,movss-prefix)]
                                    [#t @ r]))
                             (loop (cdr args) icount (+ fcount 1)
                                   (+ scount 1)
                                   (cons `(,(spill-offset (- num-spills scount 1))
                                           ,@(car args))
                                         r)))]
                          [else (error "bad arg entry:" (car args))]))])
            (%%call-native entry
                           (+ ,(assq-ref spill-labels 'spill:)
                              (* num-spills 8))
                           *amd64-call-spill-code*
                           entry
                           ,(assq-ref spill-labels 'spill:)
                           entry
                           (list* `(,func-offset ,<void*> ,ptr)
                                  `(,nfargs-offset ,<uint8> ,num-fargs)
                                  `(,init-spill-offset ,<int32> ,(* 8 num-spills))
                                  `(,epilogue-spill-offset ,<int32> ,(* 8 num-spills))
                                  patcher)
                           rettype)))))
   :port port)
  )

;;; For Windows x86_64 calling convention:
;;; https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-160

(define (gen-stub-winx64 port)
  (define reg-tmpl
    (asm-template
     '(func:    (.dataq :func)
       farg0:   (.dataq :farg0)
       farg1:   (.dataq :farg1)
       farg2:   (.dataq :farg2)
       farg3:   (.dataq :farg3)
       entry4f3:((movs_ :farg3-variant) (farg3:) %xmm3)
       entry4f2:((movs_ :farg2-variant) (farg2:) %xmm2)
       entry4f1:((movs_ :farg1-variant) (farg1:) %xmm1)
       entry4f0:((movs_ :farg0-variant) (farg0:) %xmm0)
       entry4:  (movq (imm64 :iarg3) %r9)
       entry3:  (movq (imm64 :iarg2) %r8)
       entry2:  (movq (imm64 :iarg1) %rdx)
       entry1:  (movq (imm64 :iarg0) %rcx)
       entry0:  (addq -32 %rsp)           ; %rcx-%r9 save area
                (call (func:))
                (addq 32 %rsp)
                (ret)
       end:)))

  ;; Spill case.
  (define spill-tmpl
    (asm-template
     '(func:    (.dataq :func)
       farg0:   (.dataq :farg0)
       farg1:   (.dataq :farg1)
       farg2:   (.dataq :farg2)
       farg3:   (.dataq :farg3)
       entry:
       entry4f3:((movs_ :farg3-variant) (farg3:) %xmm3)
       entry4f2:((movs_ :farg2-variant) (farg2:) %xmm2)
       entry4f1:((movs_ :farg1-variant) (farg1:) %xmm1)
       entry4f0:((movs_ :farg0-variant) (farg0:) %xmm0)
       init:    (movq (imm32 :init-spill-size) %rax)
                (leaq (spill: %rip) %rsi)
       loop:    (movq (%rsi) %rdi)
                (push %rdi)
                (addq 8 %rsi)
                (subq 8 %rax)
                (jnz loop:)
       entry4:  (movq (imm64 :iarg3) %r9)
       entry3:  (movq (imm64 :iarg2) %r8)
       entry2:  (movq (imm64 :iarg1) %rdx)
       entry1:  (movq (imm64 :iarg0) %rcx)
       entry0:  (addq -32 %rsp)           ; %rcx-%r9 save area
                (call (func:))
                (addq 32 %rsp)
       epilogue:(addq (imm32 :epilogue-spill-size) %rsp)
                (ret)
                (.align 8)
       spill:   (.dataq 0))))

  (define reg-labels  (asm-template-labels  reg-tmpl))
  (define reg-patches (asm-template-patches reg-tmpl))
  (define spill-labels  (asm-template-labels  spill-tmpl))
  (define spill-patches (asm-template-patches spill-tmpl))

  (define (entry-offsets labels)  ;; numargs -> code vector offset
    (map (cut assq-ref labels <>)
         '(entry0: entry1: entry2: entry3: entry4:
                   entry4f0: entry4f1: entry4f2: entry4f3:)))
  (define (iarg-offsets patches)   ;; iarg# -> immediate field offset
    (map (cut patch-offset <> patches)
         '(:iarg0 :iarg1 :iarg2 :iarg3)))
  (define (farg-offsets patches)   ;; farg# -> data slot offset
    (map (cut patch-offset <> patches)
         '(:farg0 :farg1 :farg2 :farg3)))
  (define (movsX-offsets patches)  ;; farg# -> movs_ prefix byte offset
    (map (cut patch-offset/maybe <> patches)
         '(:farg0-variant :farg1-variant :farg2-variant :farg3-variant)))
  (define (end-addr labels) (assq-ref labels 'end:))

  (display ";; Register-only calling" port)
  (display ";; label    offset\n" port)
  (dolist [p reg-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (pprint `(define *winx64-call-reg-code* ,(asm-template-bytes reg-tmpl))
          :port port
          ;; TRANSIENT: :radix -> :radix-prefix after the new release
          :controls (make-write-controls :pretty #t :width 75
                                         :base 16 :radix #t))
  (display ";; Spill-to-stack case" port)
  (display ";; label    offset\n" port)
  (dolist [p spill-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (pprint `(define *winx64-call-spill-code* ,(asm-template-bytes spill-tmpl))
          :port port
          ;; TRANSIENT: :radix -> :radix-prefix after the new release
          :controls (make-write-controls :pretty #t :width 75
                                         :base 16 :radix #t))

  ;; (call-winx64 <dlptr> args rettype)
  ;;  args : ((type value) ...)
  (pprint
   `(define call-winx64
      (^[ptr args rettype]
        ;; Windows x64 ABI uses shared argument count---even though
        ;; integer args and flonum args use different registers, each args
        ;; up to 4 consumes the shared slot count, and either one of
        ;; the registers is used depending on the argument type.
        ;; We still need to count fargs to determine the entry address.
        (let* ([num-args (length args)]
               [num-fargs (count (^p (%farg-type? (car p))) args)]
               [num-spills (max 0 (- num-args 4))])
          (if (zero? num-spills)
            (call-winx64-regs ptr args num-args num-fargs rettype)
            (call-winx64-spill ptr args num-args num-fargs num-spills rettype)))))
   :port port)
  (pprint
   `(define call-winx64-regs
      (let ((%%call-native (module-binding-ref 'gauche.bootstrap '%%call-native))
            (entry-offsets ',(entry-offsets reg-labels))
            (iarg-offsets  ',(iarg-offsets reg-patches))
            (farg-offsets  ',(farg-offsets reg-patches))
            (movsX-offsets ',(movsX-offsets reg-patches))
            (func-offset   ,(patch-offset :func reg-patches)))
        (^[ptr args num-args num-fargs rettype]
          (let* (;; for effective-nargs calculation, we need to consider
                 ;; unused xmm regs for preceding integral args.
                 ;; e.g. if args are int, int, double, we need up to entry4f2
                 ;; even we only have 1 fargs.
                 [effective-nargs (if (zero? num-fargs)
                                    num-args
                                    (+ 4 num-args))]
                 [entry (~ entry-offsets effective-nargs)]
                 [patcher
                  (let loop ([args args] [count 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ count 1)
                                 (cons `(,(~ iarg-offsets count)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           ;; We load both integer regs and flonum regs.
                           ;; It matters for variadic function call.
                           (loop (cdr args) (+ count 1)
                                 (cond-list
                                  [#t `(,(~ farg-offsets count) ,@(car args))]
                                  [#t `(,(~ iarg-offsets count) ,@(car args))]
                                  [(eq? (caar args) <float>)
                                   `(,(~ movsX-offsets count) ,<uint8> ,movss-prefix)]
                                  [#t @ r]))]
                          [else (error "bad arg entry:" (car args))]))])
            (%%call-native entry 0
                           *winx64-call-reg-code*
                           entry
                           ,(end-addr reg-labels)
                           entry
                           (list* `(,func-offset ,<void*> ,ptr)
                                  patcher)
                           rettype)))))
   :port port)
  (pprint
   `(define call-winx64-spill
      (let ((%%call-native (module-binding-ref 'gauche.bootstrap '%%call-native))
            (entry-offsets       ',(entry-offsets spill-labels))
            (iarg-offsets        ',(iarg-offsets spill-patches))
            (farg-offsets        ',(farg-offsets spill-patches))
            (movsX-offsets       ',(movsX-offsets spill-patches))
            (func-offset         ,(patch-offset :func spill-patches))
            (entry-addr          ,(assq-ref spill-labels 'entry:))
            (spill-offset        (^n (+ ,(assq-ref spill-labels 'spill:)
                                        (* n 8))))
            (init-spill-offset    ,(patch-offset :init-spill-size spill-patches))
            (epilogue-spill-offset ,(patch-offset :epilogue-spill-size spill-patches)))
        (^[ptr args num-args num-fargs num-spills rettype]
          (let* ([patcher
                  (let loop ([args args] [count 0] [scount 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (if (< count 4)
                             (loop (cdr args) (+ count 1) scount
                                   (cons `(,(~ iarg-offsets count)
                                           ,@(car args))
                                         r))
                             (loop (cdr args) (+ count 1)
                                   (+ scount 1)
                                   (cons `(,(spill-offset (- num-spills scount 1))
                                           ,@(car args))
                                         r)))]
                          [(%farg-type? (caar args))
                           ;; We load both integer regs and flonum regs.
                           ;; It matters for variadic function call.
                           (if (< count 4)
                             (loop (cdr args) (+ count 1) scount
                                   (cond-list
                                    [#t `(,(~ farg-offsets count) ,@(car args))]
                                    [#t `(,(~ iarg-offsets count) ,@(car args))]
                                    [(eq? (caar args) <float>)
                                     `(,(~ movsX-offsets count) ,<uint8> ,movss-prefix)]
                                    [#t @ r]))
                             (loop (cdr args) (+ count 1)
                                   (+ scount 1)
                                   (cons `(,(spill-offset (- num-spills scount 1))
                                           ,@(car args))
                                         r)))]
                          [else (error "bad arg entry:" (car args))]))])
            (%%call-native entry-addr
                           (+ ,(assq-ref spill-labels 'spill:)
                              (* num-spills 8))
                           *winx64-call-spill-code*
                           entry-addr
                           ,(assq-ref spill-labels 'spill:)
                           entry-addr
                           (list* `(,func-offset ,<void*> ,ptr)
                                  `(,init-spill-offset ,<int32> ,(* 8 num-spills))
                                  `(,epilogue-spill-offset ,<int32> ,(* 8 num-spills))
                                  patcher)
                           rettype)))))
   :port port)
  )

;;;
;;; gosh ./gen-native.scm <dir>
;;;
(define (main args)
  (match (cdr args)
    [(dir) (call-with-temporary-file
            (^[port tmpname]
              (emit-header port)
              (gen-stub-amd64 port)
              (gen-stub-winx64 port)
              (close-output-port port)
              (sys-rename tmpname #"~|dir|/native-supp.scm"))
            :directory dir :prefix "native-supp.scm")]
    [else  (exit 1 "Usage: gosh ./gen-native.scm <directory>")])
  0)
