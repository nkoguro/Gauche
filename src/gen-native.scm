;;;
;;; Generate native code vector for FFI
;;;

(use scheme.list)
(use util.match)
(use gauche.uvector)
(use file.util)
(use lang.asm.linker)
(use lang.asm.x86_64)
(use lang.asm.fragment)

;;;
;;; Common Prologue
;;;

(define (emit-header port)
  (display ";; libnative.scm supplemental code.\n" port)
  (display ";; Generated automatically by gen-native.scm.  DO NOT EDIT.\n" port)
  (display "\n" port))

;;; For SYSV AMD64 calling convention: Section 3.2 of
;;; http://refspecs.linux-foundation.org/elf/x86_64-abi-0.95.pdf

(define-asm-fragment amd64-call-reg x86_64
  '(entry6f7:   ((movs_ :farg7-variant) (farg7:) %xmm7)
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
    entry0:     (push %rbx)               ; align stack (entry has rsp%16==8)
                (movb (imm8 :num-fargs) %al)
                (call (func:))
                ;; %rax, %rdx, %xmm[01] may have the return value at this point

                ;; retkind encoding
                ;;   0 - <top>: No conversion needed
                ;;   1 - <fixnum>
                ;;   2 = integer that may not fit <fixnum>
                ;;   3 - <double>
                ;;   4 - <float>
                ;;   5 - <void>
                ;;   6 - <c-string>
                ;;   7 - general pointer
                (movb (imm8 :retkind) %bl)
                (decb %bl)
                (jsl epilog:)
                (jz fixnum:)
                (decb %bl)
                (jz integer:)
                (decb %bl)
                (jz double:)
                (decb %bl)
                (jz float:)
                (decb %bl)
                (jz void:)
                (decb %bl)
                (jz cstring:)
    pointer:    (movq %rax %rdi)
                (movq (imm64 :rettype) %rsi)
                (xorq %rax %rax)
                (call (fn-handle:))
                (jmp epilog:)
    fixnum:     (shl 2 %rax)
                (incq %rax)
                (jmp epilog:)
    integer:    (movq %rax %rdi)
                (xorq %rax %rax)
                (call (fn-int:))
                (jmp epilog:)
    double:     (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    float:      (cvtss2sd %xmm0 %xmm0)
                (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    void:       (movq (imm64 :SCM_UNDEFINED) %rax)
                (jmp epilog:)
    cstring:    (movq %rax %rdi)
                (movq -1 %rsi)
                (movq -1 %rdx)
                (movq (imm32 :SCM_STRING_COPYING) %rcx)
                (call (fn-string:)) ; cstr, -1, -1, SCM_STRING_COPYING
                ;; fallthrough
    epilog:     (pop %rbx)
                (ret)
                (.endsection text)

                (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    farg0:      (.dataq :farg0)
    farg1:      (.dataq :farg1)
    farg2:      (.dataq :farg2)
    farg3:      (.dataq :farg3)
    farg4:      (.dataq :farg4)
    farg5:      (.dataq :farg5)
    farg6:      (.dataq :farg6)
    farg7:      (.dataq :farg7)
    end:))

(define-asm-fragment amd64-call-spill x86_64
  '(entry6f7:   ((movs_ :farg7-variant) (farg7:) %xmm7)
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
                (leaq (spill:) %rsi)
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
                ;; %rax, %rdx, %xmm[01] may have the return value at this point

                ;; retkind encoding
                ;;   0 - <top>: No conversion needed
                ;;   1 - <fixnum>
                ;;   2 = integer that may not fit <fixnum>
                ;;   3 - <double>
                ;;   4 - <float>
                ;;   5 - <void>
                ;;   6 - <c-string>
                ;;   7 - general pointer
                (movb (imm8 :retkind) %bl)
                (decb %bl)
                (jsl epilog:)
                (jz fixnum:)
                (decb %bl)
                (jz integer:)
                (decb %bl)
                (jz double:)
                (decb %bl)
                (jz float:)
                (decb %bl)
                (jz void:)
                (decb %bl)
                (jz cstring:)
    pointer:    (movq %rax %rdi)
                (movq (imm64 :rettype) %rsi)
                (xorq %rax %rax)
                (call (fn-handle:))
                (jmp epilog:)
    fixnum:     (shl 2 %rax)
                (incq %rax)
                (jmp epilog:)
    integer:    (movq %rax %rdi)
                (xorq %rax %rax)
                (call (fn-int:))
                (jmp epilog:)
    double:     (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    float:      (cvtss2sd %xmm0 %xmm0)
                (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    void:       (movq (imm64 :SCM_UNDEFINED) %rax)
                (jmp epilog:)
    cstring:    (movq %rax %rdi)
                (movq -1 %rsi)
                (movq -1 %rdx)
                (movq (imm32 :SCM_STRING_COPYING) %rcx)
                (call (fn-string:)) ; cstr, -1, -1, SCM_STRING_COPYING
                ;; fallthrough
    epilog:     (addq (imm32 :epilogue-spill-size) %rsp)
                (ret)
                (.endsection text)

                (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    farg0:      (.dataq :farg0)
    farg1:      (.dataq :farg1)
    farg2:      (.dataq :farg2)
    farg3:      (.dataq :farg3)
    farg4:      (.dataq :farg4)
    farg5:      (.dataq :farg5)
    farg6:      (.dataq :farg6)
    farg7:      (.dataq :farg7)
    spill:      (.dataq :spill)))

;;; For Windows x86_64 calling convention:
;;; https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-160

(define-asm-fragment winx64-call-reg x86_64
  '(entry4f3:((movs_ :farg3-variant) (farg3:) %xmm3)
    entry4f2:((movs_ :farg2-variant) (farg2:) %xmm2)
    entry4f1:((movs_ :farg1-variant) (farg1:) %xmm1)
    entry4f0:((movs_ :farg0-variant) (farg0:) %xmm0)
    entry4:  (movq (imm64 :iarg3) %r9)
    entry3:  (movq (imm64 :iarg2) %r8)
    entry2:  (movq (imm64 :iarg1) %rdx)
    entry1:  (movq (imm64 :iarg0) %rcx)
    entry0:  (push %rbx)               ; save rbx; aligns rsp to 16
             (addq -32 %rsp)           ; shadow space (shared with helper calls)
             (call (func:))
             ;; %rax or %xmm0 may have the return value at this point.
             ;; retkind encoding matches amd64-call-reg.
             (movb (imm8 :retkind) %bl)
             (decb %bl)
             (jsl epilog:)             ; retkind=0: return %rax as-is
             (jz fixnum:)              ; retkind=1
             (decb %bl)
             (jz integer:)             ; retkind=2
             (decb %bl)
             (jz double:)              ; retkind=3
             (decb %bl)
             (jz float:)               ; retkind=4
             (decb %bl)
             (jz void:)                ; retkind=5
             (decb %bl)
             (jz cstring:)             ; retkind=6
    pointer: (movq %rax %rcx)          ; rcx=ptr
             (movq (imm64 :rettype) %rdx) ; rdx=type
             (call (fn-handle:))
             (jmp epilog:)
    fixnum:  (shl 2 %rax)
             (incq %rax)
             (jmp epilog:)
    integer: (movq %rax %rcx)          ; rcx=intptr_t
             (call (fn-int:))
             (jmp epilog:)
    float:   (cvtss2sd %xmm0 %xmm0)
    double:  (call (fn-flonum:))
             (jmp epilog:)
    void:    (movq (imm64 :SCM_UNDEFINED) %rax)
             (jmp epilog:)
    cstring: (movq %rax %rcx)          ; rcx=cstr
             (movq -1 %rdx)            ; rdx=size
             (movq -1 %r8)             ; r8=len
             (movq (imm32 :SCM_STRING_COPYING) %r9) ; r9=flags
             (call (fn-string:))
    epilog:  (addq 32 %rsp)
             (pop %rbx)
             (ret)
             (.endsection text)

             (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    farg0:   (.dataq :farg0)
    farg1:   (.dataq :farg1)
    farg2:   (.dataq :farg2)
    farg3:   (.dataq :farg3)
    end:))

(define-asm-fragment winx64-call-spill x86_64
  '(entry:
    entry4f3:((movs_ :farg3-variant) (farg3:) %xmm3)
    entry4f2:((movs_ :farg2-variant) (farg2:) %xmm2)
    entry4f1:((movs_ :farg1-variant) (farg1:) %xmm1)
    entry4f0:((movs_ :farg0-variant) (farg0:) %xmm0)
    init:    (push %rbx)               ; save rbx before spill pushes
             (movq (imm32 :init-spill-size) %rax)
             (leaq (spill: %rip) %r10) ; %r10/%r11: volatile in Win64 ABI
             (subq (imm8 :align-pad) %rsp) ; ensure alignment
    loop:    (movq (%r10) %r11)
             (push %r11)
             (addq 8 %r10)
             (subq 8 %rax)
             (jnz loop:)
    entry4:  (movq (imm64 :iarg3) %r9)
    entry3:  (movq (imm64 :iarg2) %r8)
    entry2:  (movq (imm64 :iarg1) %rdx)
    entry1:  (movq (imm64 :iarg0) %rcx)
    entry0:  (addq -32 %rsp)           ; shadow space (shared with helper calls)
             (call (func:))
             ;; boxing dispatch (Windows x64 calling convention for helpers)
             (movb (imm8 :retkind) %bl)
             (decb %bl)
             (jsl epilog:)
             (jz fixnum:)
             (decb %bl)
             (jz integer:)
             (decb %bl)
             (jz double:)
             (decb %bl)
             (jz float:)
             (decb %bl)
             (jz void:)
             (decb %bl)
             (jz cstring:)
    pointer: (movq %rax %rcx)
             (movq (imm64 :rettype) %rdx)
             (call (fn-handle:))
             (jmp epilog:)
    fixnum:  (shl 2 %rax)
             (incq %rax)
             (jmp epilog:)
    integer: (movq %rax %rcx)
             (call (fn-int:))
             (jmp epilog:)
    float:   (cvtss2sd %xmm0 %xmm0)
    double:  (call (fn-flonum:))
             (jmp epilog:)
    void:    (movq (imm64 :SCM_UNDEFINED) %rax)
             (jmp epilog:)
    cstring: (movq %rax %rcx)
             (movq -1 %rdx)
             (movq -1 %r8)
             (movq (imm32 :SCM_STRING_COPYING) %r9)
             (call (fn-string:))
    epilog:  (addq 32 %rsp)
    epilogue:(addq (imm32 :epilogue-spill-size) %rsp) ; undo spills+pad before rbx
             (pop %rbx)
             (ret)
             (.endsection text)

             (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    farg0:   (.dataq :farg0)
    farg1:   (.dataq :farg1)
    farg2:   (.dataq :farg2)
    farg3:   (.dataq :farg3)
    spill:   (.dataq :spill)))

;;;
;;; Code generators
;;;

(define (gen-stub-amd64 port)
  (define (Ps . exprs)
    (for-each (cut pprint <> :port port) exprs))

  (display ";; Register-only calling\n" port)
  (dump-asm-fragment amd64-call-reg port)

  (display ";; Spill-to-stack case\n" port)
  (dump-asm-fragment amd64-call-spill port)

  (Ps
   `(define (%iarg-type? t)
      (or (eq? t <top>)
          (subtype? t <integer>)
          (eq? t <c-string>)
          (is-a? t <c-pointer>)
          (is-a? t <c-array>)
          (is-a? t <c-function>)))
   `(define (%farg-type? t)
      (or (eq? t <double>)
          (eq? t <float>)))
   ;; Returns the retkind integer for the asm conversion dispatch.
   ;; Encoding must match the branch code in the assembly listing
   ;; in gen-native.scm.
   `(define (%asm-retkind rettype)
      (cond [(eq? rettype <top>)                                  0]
            [(eq? rettype <uint8>)                                1]
            [(eq? rettype <intptr_t>)                             2]
            [(eq? rettype <double>)                               3]
            [(eq? rettype <float>)                                4]
            [(eq? rettype <void>)                                 5]
            [(eq? rettype <c-string>)                             6]
            [(or (is-a? rettype <c-pointer>) (is-a? rettype <c-array>)) 7]
            [else (error "unknown FFI return type for asm path:" rettype)]))
   `(define-enum SCM_STRING_COPYING)
   )

  ;; (call-amd64 <native-handle> args rettype)
  ;;  args : ((type value) ...)
  ;; NB: In the final form, we won't expose this function to the user; it's
  ;; too error-prone.  You can wreck havoc just by passing a wrong type.
  ;; Instead, we'll require the user to parse the C function declaration
  ;; and we automatically extract the type info.
  (Ps
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
                              num-spills rettype))))))

  ;; call-amd64-regs: helper addresses prelinked at init! time; per-call
  ;; params supply :func, argument values, :retkind, and :rettype.
  (Ps
   `(define call-amd64-regs
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (amd64-call-reg-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-iargs num-fargs rettype]
          (when (not link-tmpl) (init!))
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry-label (~ '#(entry0: entry1: entry2: entry3: entry4:
                                    entry5: entry6:
                                    entry6f0: entry6f1: entry6f2: entry6f3:
                                    entry6f4: entry6f5: entry6f6: entry6f7:)
                                 effective-nargs)]
                 [retkind (%asm-retkind rettype)]
                 [params
                  (let loop ([args args] [icount 0] [fcount 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ icount 1) fcount
                                 (cons `(,(~ '#(:iarg0 :iarg1 :iarg2
                                                :iarg3 :iarg4 :iarg5)
                                             icount)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3
                                             :farg4 :farg5 :farg6 :farg7)
                                          fcount)]
                                 [vkey (~ '#(:farg0-variant :farg1-variant
                                             :farg2-variant :farg3-variant
                                             :farg4-variant :farg5-variant
                                             :farg6-variant :farg7-variant)
                                          fcount)])
                             (loop (cdr args) icount (+ fcount 1)
                                   (if (eq? (caar args) <float>)
                                     (list* `(,vkey ,<uint8> movss)
                                            `(,fkey ,@(car args)) r)
                                     (cons `(,fkey ,@(car args)) r))))]
                          [else (error "bad arg entry:" (car args))]))])
            (receive [bytes lbs]
                (link-tmpl (list prelinked-tmpl)
                           `((:func ,<void*> ,ptr)
                             (:num-fargs ,<uint8> ,num-fargs)
                             (:retkind ,<integer> ,retkind)
                             (:rettype ,<top> ,rettype)
                             ,@params))
              ((% '%%call-native) 0 0 bytes 0
                                  (lbl-off lbs 'end:)
                                  (lbl-off lbs entry-label)
                                  0 0)))))))

  ;; call-amd64-spill: named patches handled by link-templates; only raw
  ;; spill-slot offsets remain in the %%call-native patcher list.
  ;; The four C helper addresses and SCM_STRING_COPYING are baked into a
  ;; prelinked template once at init! time.  Per-call params supply :func,
  ;; argument values, :retkind, and :rettype.
  (Ps
   `(define call-amd64-spill
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (amd64-call-spill-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-iargs num-fargs num-spills rettype]
          (when (not link-tmpl) (init!))
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry-label (~ '#(entry0: entry1: entry2: entry3: entry4:
                                    entry5: entry6:
                                    entry6f0: entry6f1: entry6f2: entry6f3:
                                    entry6f4: entry6f5: entry6f6: entry6f7:)
                                 effective-nargs)])
            (let loop ([args args] [icount 0] [fcount 0] [scount 0]
                       [named '()] [spill-params '()])
              (if (null? args)
                (let* ([align-pad (if (even? num-spills) 8 0)]
                       [spill-area-bytes (* 8 num-spills)]
                       [retkind (%asm-retkind rettype)])
                  (receive [bytes lbs]
                      (link-tmpl (list prelinked-tmpl)
                                 `((:func ,<void*> ,ptr)
                                   (:num-fargs ,<uint8> ,num-fargs)
                                   (:init-spill-size
                                    ,<int32>
                                    ,spill-area-bytes)
                                   (:epilogue-spill-size
                                    ,<int32>
                                    ,(+ spill-area-bytes align-pad))
                                   (:align-pad ,<int8> ,align-pad)
                                   (:retkind ,<integer> ,retkind)
                                   (:rettype ,<top> ,rettype)
                                   ,@named
                                   ,@spill-params)
                                 :postamble spill-area-bytes)
                    ((% '%%call-native) 0         ;tstart
                                        0         ;tend (no zero fill)
                                        bytes     ;code
                                        0         ;start
                                        (+ (lbl-off lbs 'spill:)
                                           spill-area-bytes) ;end
                                        (lbl-off lbs entry-label) ;entry
                                        0 0)))  ;win-prolog-end win-frame-size
                (cond [(%iarg-type? (caar args))
                       (if (< icount 6)
                         (loop (cdr args) (+ icount 1) fcount scount
                               (cons `(,(~ '#(:iarg0 :iarg1 :iarg2
                                             :iarg3 :iarg4 :iarg5)
                                           icount)
                                       ,@(car args))
                                     named)
                               spill-params)
                         (loop (cdr args) (+ icount 1) fcount (+ scount 1)
                               named
                               (cons `(:spill ,@(car args)
                                       ,(* (- num-spills scount 1) 8))
                                     spill-params)))]
                      [(%farg-type? (caar args))
                       (if (< fcount 8)
                         (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3
                                           :farg4 :farg5 :farg6 :farg7)
                                        fcount)]
                               [vkey (~ '#(:farg0-variant :farg1-variant
                                           :farg2-variant :farg3-variant
                                           :farg4-variant :farg5-variant
                                           :farg6-variant :farg7-variant)
                                        fcount)])
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
                      [else (error "bad arg entry:" (car args))]))))))))
  )

(define (gen-stub-winx64 port)
  (define (Ps . exprs)
    (for-each (cut pprint <> :port port) exprs))

  (display ";; Register-only calling\n" port)
  (dump-asm-fragment winx64-call-reg port)

  (display ";; Spill-to-stack case\n" port)
  (dump-asm-fragment winx64-call-spill port)

  ;; (call-winx64 <native-handle> args rettype)
  ;;  args : ((type value) ...)
  (Ps
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
            (call-winx64-spill ptr args num-args num-fargs num-spills rettype))))))

  (Ps
   `(define call-winx64-regs
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (winx64-call-reg-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-args num-fargs rettype]
          (when (not link-tmpl) (init!))
          (let* (;; for effective-nargs calculation, we need to consider
                 ;; unused xmm regs for preceding integral args.
                 ;; e.g. if args are int, int, double, we need up to entry4f2
                 ;; even we only have 1 fargs.
                 [effective-nargs (if (zero? num-fargs)
                                    num-args
                                    (+ 4 num-args))]
                 [entry-label (~ '#(entry0: entry1: entry2: entry3: entry4:
                                    entry4f0: entry4f1: entry4f2: entry4f3:)
                                 effective-nargs)]
                 [retkind (%asm-retkind rettype)]
                 [params
                  (let loop ([args args] [count 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ count 1)
                                 (cons `(,(~ '#(:iarg0 :iarg1 :iarg2 :iarg3)
                                             count)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           ;; We load both integer regs and flonum regs.
                           ;; It matters for variadic function call.
                           (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3) count)]
                                 [ikey (~ '#(:iarg0 :iarg1 :iarg2 :iarg3) count)]
                                 [vkey (~ '#(:farg0-variant :farg1-variant
                                             :farg2-variant :farg3-variant)
                                          count)])
                             (loop (cdr args) (+ count 1)
                                   (if (eq? (caar args) <float>)
                                     (list* `(,vkey ,<uint8> movss)
                                            `(,fkey ,@(car args))
                                            `(,ikey ,@(car args)) r)
                                     (list* `(,fkey ,@(car args))
                                            `(,ikey ,@(car args)) r))))]
                          [else (error "bad arg entry:" (car args))]))])
            (receive [bytes lbs]
                (link-tmpl (list prelinked-tmpl)
                           (list* `(:func ,<void*> ,ptr)
                                  `(:retkind ,<integer> ,retkind)
                                  `(:rettype ,<top> ,rettype)
                                  params))
              ;; win-frame-size=40: push %rbx (8) + shadow space (32)
              ;; Prolog ends after "push %rbx" (1 byte) + "addq -32 %rsp" (4 bytes)
              ((% '%%call-native) 0 0 bytes 0
                                  (lbl-off lbs 'end:)
                                  (lbl-off lbs entry-label)
                                  (+ (lbl-off lbs 'entry0:) 5) 40)))))))

  (Ps
   `(define call-winx64-spill
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (winx64-call-spill-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-args num-fargs num-spills rettype]
          (when (not link-tmpl) (init!))
          (let loop ([args args] [count 0] [scount 0] [named '()] [spill-params '()])
            (if (null? args)
              ;; With push %rbx at entry0, alignment parity is flipped.
              ;; We need rsp%16==0 before "call func:", which requires
              ;; align-pad=(if (even? num-spills) 0 8).
              (let* ([align-pad (if (even? num-spills) 0 8)]
                     [spill-area-bytes (* 8 num-spills)]
                     [retkind (%asm-retkind rettype)])
                (receive [bytes lbs]
                    (link-tmpl (list prelinked-tmpl)
                               `((:func ,<void*> ,ptr)
                                 (:init-spill-size
                                  ,<int32>
                                  ,spill-area-bytes)
                                 (:epilogue-spill-size
                                  ,<int32>
                                  ,(+ spill-area-bytes align-pad))
                                 (:align-pad ,<int8> ,align-pad)
                                 (:retkind ,<integer> ,retkind)
                                 (:rettype ,<top> ,rettype)
                                 ,@named
                                 ,@spill-params)
                               :postamble spill-area-bytes)
                  ;; win-frame-size = push %rbx (8) + shadow (32) + spill+pad
                  ;; Prolog ends after "addq -32 %rsp" (4) at entry0:
                  ;; (push %rbx is now in init:, before the spill loop)
                  ((% '%%call-native) 0      ;tstart
                                      0      ;tend (no zero fill needed)
                                      bytes  ;code
                                      0      ;start
                                      (+ (lbl-off lbs 'spill:)
                                         spill-area-bytes)  ;end
                                      (lbl-off lbs 'entry:) ;entry
                                      (+ (lbl-off lbs 'entry0:) 4)
                                      (+ spill-area-bytes align-pad 40))))
              (cond [(%iarg-type? (caar args))
                     (if (< count 4)
                       (loop (cdr args) (+ count 1) scount
                             (cons `(,(~ '#(:iarg0 :iarg1 :iarg2 :iarg3) count)
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
                       (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3) count)]
                             [ikey (~ '#(:iarg0 :iarg1 :iarg2 :iarg3) count)]
                             [vkey (~ '#(:farg0-variant :farg1-variant
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
                             (cons `(:spill ,@(car args)
                                     ,(* (- num-spills scount 1) 8))
                                   spill-params)))]
                    [else (error "bad arg entry:" (car args))])))))))
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
