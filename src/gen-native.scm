;;;
;;; Generate native code vector for FFI
;;;

(use scheme.list)
(use util.match)
(use gauche.uvector)
(use file.util)
(use lang.asm.object)
(use lang.asm.x86_64)

;;;
;;; Common Prologue
;;;

(define (emit-header port)
  (display ";; libnative.scm supplemental code.\n" port)
  (display ";; Generated automatically by gen-native.scm.  DO NOT EDIT.\n" port)
  (display "\n" port))

;;; Emit bytes, labels, and patches of TMPL as three top-level define forms.
;;; PREFIX is a string; the variables become *<prefix>-bytes*,
;;; *<prefix>-labels*, and *<prefix>-patches*.
(define (emit-tmpl-vars port prefix tmpl)
  (let ([bytes-var   (string->symbol #"*~|prefix|-bytes*")]
        [labels-var  (string->symbol #"*~|prefix|-labels*")]
        [patches-var (string->symbol #"*~|prefix|-patches*")]
        [endian-var  (string->symbol #"*~|prefix|-endian*")])
    (pprint `(define ,bytes-var ,(~ tmpl'bytes))
            :port port
            ;; TRANSIENT: :radix -> :radix-prefix after the new release
            :controls (make-write-controls :pretty #t :width 75
                                           :base 16 :radix #t))
    (pprint `(define ,labels-var ',(~ tmpl'labels)) :port port)
    (pprint `(define ,patches-var ',(~ tmpl'patches)) :port port)
    (pprint `(define ,endian-var ',(~ tmpl'endian)) :port port)))

;;; For SYSV AMD64 calling convention: Section 3.2 of
;;; http://refspecs.linux-foundation.org/elf/x86_64-abi-0.95.pdf

(define (gen-stub-amd64 port)
  ;; When all args can be on registers.
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
                   (subq (imm8 :align-pad) %rsp)
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
       spill:      (.dataq :spill))))

  (define reg-labels   (~ reg-tmpl'labels))
  (define spill-labels (~ spill-tmpl'labels))

  ;; Print label/offset comments for reference.
  (display ";; Register-only calling" port)
  (display ";; label    offset\n" port)
  (dolist [p reg-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (emit-tmpl-vars port "amd64-call-reg" reg-tmpl)

  (display ";; Spill-to-stack case" port)
  (display ";; label    offset\n" port)
  (dolist [p spill-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (emit-tmpl-vars port "amd64-call-spill" spill-tmpl)

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

  (pprint
   '(define-constant movss-prefix #xf3)  ; movss opcode prefix (vs movsd's #xf2)
   :port port)

  ;; (call-amd64 <native-handle> args rettype)
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

  ;; call-amd64-regs: uses link-template to apply all named patches; passes
  ;; the fully-patched bytes to %%call-native with an empty patcher list.
  (pprint
   `(define call-amd64-regs
      (let ((%%call-native (module-binding-ref 'gauche.bootstrap '%%call-native))
            (tmpl #f) (link-tmpl #f) (entry-offsets #f) (end-addr #f))
        (define (init!)
          (let* ([t   ((module-binding-ref 'lang.asm.object 'make-obj-template)
                       *amd64-call-reg-bytes*
                       *amd64-call-reg-labels*
                       *amd64-call-reg-patches*
                       *amd64-call-reg-endian*)]
                 [lbs (~ t'labels)])
            (set! tmpl t)
            (set! link-tmpl (module-binding-ref 'lang.asm.object 'link-template))
            (set! entry-offsets
                  (map (^k (cdr (assq k lbs)))
                       '(entry0: entry1: entry2: entry3: entry4: entry5: entry6:
                         entry6f0: entry6f1: entry6f2: entry6f3:
                         entry6f4: entry6f5: entry6f6: entry6f7:)))
            (set! end-addr (cdr (assq 'end: lbs)))))
        (^[ptr args num-iargs num-fargs rettype]
          (when (not tmpl) (init!))
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry (~ entry-offsets effective-nargs)]
                 [params
                  (let loop ([args args] [icount 0] [fcount 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ icount 1) fcount
                                 (cons `(,(~ '(:iarg0 :iarg1 :iarg2 :iarg3 :iarg4 :iarg5)
                                             icount)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           (let ([fkey (~ '(:farg0 :farg1 :farg2 :farg3
                                            :farg4 :farg5 :farg6 :farg7) fcount)]
                                 [vkey (~ '(:farg0-variant :farg1-variant
                                            :farg2-variant :farg3-variant
                                            :farg4-variant :farg5-variant
                                            :farg6-variant :farg7-variant) fcount)])
                             (loop (cdr args) icount (+ fcount 1)
                                   (if (eq? (caar args) <float>)
                                     (list* `(,vkey ,<uint8> movss)
                                            `(,fkey ,@(car args)) r)
                                     (cons `(,fkey ,@(car args)) r))))]
                          [else (error "bad arg entry:" (car args))]))])
            (receive [bytes _]
                (link-tmpl tmpl
                           `((:func ,<void*> ,ptr)
                             (:num-fargs ,<uint8> ,num-fargs)
                             ,@params))
              (%%call-native 0 0 bytes 0 end-addr entry rettype 0 0))))))
   :port port)

  ;; call-amd64-spill: named patches handled by link-template; only raw
  ;; spill-slot offsets remain in the %%call-native patcher list.
  (pprint
   `(define call-amd64-spill
      (let ((%%call-native (module-binding-ref 'gauche.bootstrap '%%call-native))
            (tmpl #f) (link-tmpl #f) (entry-offsets #f) (spill-base #f))
        (define (init!)
          (let* ([t   ((module-binding-ref 'lang.asm.object 'make-obj-template)
                       *amd64-call-spill-bytes*
                       *amd64-call-spill-labels*
                       *amd64-call-spill-patches*
                       *amd64-call-spill-endian*)]
                 [lbs (~ t'labels)])
            (set! tmpl t)
            (set! link-tmpl (module-binding-ref 'lang.asm.object 'link-template))
            (set! entry-offsets
                  (map (^k (cdr (assq k lbs)))
                       '(entry0: entry1: entry2: entry3: entry4: entry5: entry6:
                         entry6f0: entry6f1: entry6f2: entry6f3:
                         entry6f4: entry6f5: entry6f6: entry6f7:)))
            (set! spill-base (cdr (assq 'spill: lbs)))))
        (^[ptr args num-iargs num-fargs num-spills rettype]
          (when (not tmpl) (init!))
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry (~ entry-offsets effective-nargs)])
            (let loop ([args args] [icount 0] [fcount 0] [scount 0]
                       [named '()] [spill-params '()])
              (if (null? args)
                (let* ([align-pad (if (even? num-spills) 8 0)]
                       [spill-area-bytes (* 8 num-spills)])
                  (receive [bytes _]
                      (link-tmpl tmpl
                                 `((:func ,<void*> ,ptr)
                                   (:num-fargs ,<uint8> ,num-fargs)
                                   (:init-spill-size
                                    ,<int32>
                                    ,spill-area-bytes)
                                   (:epilogue-spill-size
                                    ,<int32>
                                    ,(+ spill-area-bytes align-pad))
                                   (:align-pad ,<int8> ,align-pad)
                                   ,@named
                                   ,@spill-params)
                                 :postamble spill-area-bytes)
                    (%%call-native 0     ;tstart
                                   0     ;tend (no zero fill)
                                   bytes ;code
                                   0     ;start
                                   (+ spill-base spill-area-bytes) ;end
                                   entry ;entry
                                   rettype
                                   0 0)))
                (cond [(%iarg-type? (caar args))
                       (if (< icount 6)
                         (loop (cdr args) (+ icount 1) fcount scount
                               (cons `(,(~ '(:iarg0 :iarg1 :iarg2
                                            :iarg3 :iarg4 :iarg5) icount)
                                       ,@(car args))
                                     named)
                               spill-params)
                         (loop (cdr args) (+ icount 1) fcount (+ scount 1)
                               named
                               (cons `(:func ,@(car args)
                                       ,(+ spill-base (* (- num-spills scount 1) 8)))
                                     spill-params)))]
                      [(%farg-type? (caar args))
                       (if (< fcount 8)
                         (let ([fkey (~ '(:farg0 :farg1 :farg2 :farg3
                                         :farg4 :farg5 :farg6 :farg7) fcount)]
                               [vkey (~ '(:farg0-variant :farg1-variant
                                          :farg2-variant :farg3-variant
                                          :farg4-variant :farg5-variant
                                          :farg6-variant :farg7-variant) fcount)])
                           (loop (cdr args) icount (+ fcount 1) scount
                                 (if (eq? (caar args) <float>)
                                   (list* `(,vkey ,<uint8> movss)
                                          `(,fkey ,@(car args)) named)
                                   (cons `(,fkey ,@(car args)) named))
                                 spill-params))
                         (loop (cdr args) icount (+ fcount 1) (+ scount 1)
                               named
                               (cons `(:spill ,@(car args)
                                       ,(* (- num-spills scount 1) 8))
                                     spill-params)))]
                      [else (error "bad arg entry:" (car args))])))))))
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
       entry0:  (addq -40 %rsp)           ; %rcx-%r9 save area + 8byte align
                (call (func:))
                (addq 40 %rsp)
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
                (subq (imm8 :align-pad) %rsp) ; ensure alignment
       loop:    (movq (%rsi) %rdi)
                (push %rdi)
                (addq 8 %rsi)
                (subq 8 %rax)
                (jnz loop:)
       entry4:  (movq (imm64 :iarg3) %r9)
       entry3:  (movq (imm64 :iarg2) %r8)
       entry2:  (movq (imm64 :iarg1) %rdx)
       entry1:  (movq (imm64 :iarg0) %rcx)
       entry0:  (addq -32 %rsp)         ; %rcx-%r9 save area
                (call (func:))
                (addq 32 %rsp)
       epilogue:(addq (imm32 :epilogue-spill-size) %rsp)
                (ret)
                (.align 8)
       spill:   (.dataq :spill))))

  (define reg-labels   (~ reg-tmpl'labels))
  (define spill-labels (~ spill-tmpl'labels))

  (display ";; Register-only calling" port)
  (display ";; label    offset\n" port)
  (dolist [p reg-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (emit-tmpl-vars port "winx64-call-reg" reg-tmpl)

  (display ";; Spill-to-stack case" port)
  (display ";; label    offset\n" port)
  (dolist [p spill-labels]
    (format port ";; ~10a  ~3d\n" (car p) (cdr p)))
  (emit-tmpl-vars port "winx64-call-spill" spill-tmpl)

  ;; (call-winx64 <native-handle> args rettype)
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
            (tmpl #f) (link-tmpl #f) (entry-offsets #f) (end-addr #f)
            (win-prolog-end #f))
        (define (init!)
          (let* ([t   ((module-binding-ref 'lang.asm.object 'make-obj-template)
                       *winx64-call-reg-bytes*
                       *winx64-call-reg-labels*
                       *winx64-call-reg-patches*
                       *winx64-call-reg-endian*)]
                 [lbs (~ t'labels)])
            (set! tmpl t)
            (set! link-tmpl (module-binding-ref 'lang.asm.object 'link-template))
            (set! entry-offsets
                  (map (^k (cdr (assq k lbs)))
                       '(entry0: entry1: entry2: entry3: entry4:
                         entry4f0: entry4f1: entry4f2: entry4f3:)))
            (set! end-addr (cdr (assq 'end: lbs)))
            ;; Prolog ends after the 4-byte "addq -40 %rsp" at entry0:
            (set! win-prolog-end (+ (cdr (assq 'entry0: lbs)) 4))))
        (^[ptr args num-args num-fargs rettype]
          (when (not tmpl) (init!))
          (let* (;; for effective-nargs calculation, we need to consider
                 ;; unused xmm regs for preceding integral args.
                 ;; e.g. if args are int, int, double, we need up to entry4f2
                 ;; even we only have 1 fargs.
                 [effective-nargs (if (zero? num-fargs)
                                    num-args
                                    (+ 4 num-args))]
                 [entry (~ entry-offsets effective-nargs)]
                 [params
                  (let loop ([args args] [count 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ count 1)
                                 (cons `(,(~ '(:iarg0 :iarg1 :iarg2 :iarg3) count)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           ;; We load both integer regs and flonum regs.
                           ;; It matters for variadic function call.
                           (let ([fkey (~ '(:farg0 :farg1 :farg2 :farg3) count)]
                                 [ikey (~ '(:iarg0 :iarg1 :iarg2 :iarg3) count)]
                                 [vkey (~ '(:farg0-variant :farg1-variant
                                            :farg2-variant :farg3-variant) count)])
                             (loop (cdr args) (+ count 1)
                                   (if (eq? (caar args) <float>)
                                     (list* `(,vkey ,<uint8> movss)
                                            `(,fkey ,@(car args))
                                            `(,ikey ,@(car args)) r)
                                     (list* `(,fkey ,@(car args))
                                            `(,ikey ,@(car args)) r))))]
                          [else (error "bad arg entry:" (car args))]))])
            (receive [bytes _]
                (link-tmpl tmpl
                           (list* `(:func ,<void*> ,ptr)
                                  params))
              ;; win-frame-size=40: shadow space (32) + 8-byte alignment
              (%%call-native 0 0 bytes 0 end-addr entry rettype
                             win-prolog-end 40))))))
   :port port)

  (pprint
   `(define call-winx64-spill
      (let ((%%call-native (module-binding-ref 'gauche.bootstrap '%%call-native))
            (tmpl #f) (link-tmpl #f) (entry-addr #f) (spill-base #f)
            (win-prolog-end #f))
        (define (init!)
          (let* ([t   ((module-binding-ref 'lang.asm.object 'make-obj-template)
                       *winx64-call-spill-bytes*
                       *winx64-call-spill-labels*
                       *winx64-call-spill-patches*
                       *winx64-call-spill-endian*)]
                 [lbs (~ t'labels)])
            (set! tmpl t)
            (set! link-tmpl (module-binding-ref 'lang.asm.object 'link-template))
            (set! entry-addr (cdr (assq 'entry: lbs)))
            (set! spill-base (cdr (assq 'spill: lbs)))
            ;; Prolog ends after the 4-byte "addq -32 %rsp" at entry0:
            (set! win-prolog-end (+ (cdr (assq 'entry0: lbs)) 4))))
        (^[ptr args num-args num-fargs num-spills rettype]
          (when (not tmpl) (init!))
          (let loop ([args args] [count 0] [scount 0] [named '()] [spill-params '()])
            (if (null? args)
              (let* ([align-pad (if (even? num-spills) 8 0)]
                     [spill-area-bytes (* 8 num-spills)])
                (receive [bytes _]
                    (link-tmpl tmpl
                               `((:func ,<void*> ,ptr)
                                 (:init-spill-size
                                  ,<int32>
                                  ,spill-area-bytes)
                                 (:epilogue-spill-size
                                  ,<int32>
                                  ,(+ spill-area-bytes align-pad))
                                 (:align-pad ,<int8> ,align-pad)
                                 ,@named
                                 ,@spill-params)
                               :postamble spill-area-bytes)
                  ;; win-frame-size = shadow space (32) + spill args (8*N)
                  (%%call-native 0      ;tstart
                                 0      ;tend (no zero fill needed)
                                 bytes  ;code
                                 0      ;start
                                 (+ spill-base spill-area-bytes) ;end
                                 entry-addr                      ;entry
                                 rettype
                                 win-prolog-end
                                 (+ spill-area-bytes align-pad 32))))
              (cond [(%iarg-type? (caar args))
                     (if (< count 4)
                       (loop (cdr args) (+ count 1) scount
                             (cons `(,(~ '(:iarg0 :iarg1 :iarg2 :iarg3) count)
                                     ,@(car args))
                                   named)
                             spill-params)
                       (loop (cdr args) (+ count 1) (+ scount 1)
                             named
                             (cons `(:spill ,@(car args)
                                     ,(* (- num-spills scount 1) 8))
                                   spill-params)))]
                    [(%farg-type? (caar args))
                     ;; We load both integer regs and flonum regs.
                     ;; It matters for variadic function call.
                     (if (< count 4)
                       (let ([fkey (~ '(:farg0 :farg1 :farg2 :farg3) count)]
                             [ikey (~ '(:iarg0 :iarg1 :iarg2 :iarg3) count)]
                             [vkey (~ '(:farg0-variant :farg1-variant
                                        :farg2-variant :farg3-variant) count)])
                         (loop (cdr args) (+ count 1) scount
                               (if (eq? (caar args) <float>)
                                 (list* `(,vkey ,<uint8> movss)
                                        `(,fkey ,@(car args))
                                        `(,ikey ,@(car args)) named)
                                 (list* `(,fkey ,@(car args))
                                        `(,ikey ,@(car args)) named))
                               spill-params))
                       (loop (cdr args) (+ count 1) (+ scount 1)
                             named
                             (cons `(:func ,@(car args)
                                     ,(+ spill-base (* (- num-spills scount 1) 8)))
                                   spill-params)))]
                    [else (error "bad arg entry:" (car args))]))))))
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
