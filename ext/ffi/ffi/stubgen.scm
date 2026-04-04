;;;
;;; gauche.ffi.stubgen - Foreign function interface via stub generation
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.ffi.stubgen
  (use gauche.ffi)
  (use gauche.cgen)
  (use gauche.cgen.type)
  (use gauche.cgen.dyncomp)
  (use gauche.ctype)
  (use gauche.sequence)
  (use gauche.config)
  (use gauche.package.compile)
  (use file.util)
  (use util.match)
  (export with-stubgen-ffi))
(select-module gauche.ffi.stubgen)

(define-syntax with-stubgen-ffi
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ dlo-expr options cfn-instances body)
        (quasirename r
          `(begin
             (define _dummy
               (compile-and-link-ffi-stub ,dlo-expr
                                          ',cfn-instances
                                          (current-module)))
             ,@body))]))))

(define (compile-and-link-ffi-stub dlobj cfn-instances mod)
  (let ([unit (generate-ffi-c-code-unit cfn-instances)])
    (cgen-dynamic-load unit (sys-tmpdir))
    ((module-binding-ref mod 'ffisetup) dlobj)))

;;;
;;; Type helpers operating on <native-type> instances
;;;
;;; All type information has already been resolved to <native-type> by
;;; parse-define-c-function in gauche.ffi.  These helpers extract the
;;; C code fragments needed to emit the stub.
;;;

;;; True if a native type is compound (pointer, array, struct, union,
;;; or function pointer).  Compound types are passed as native handles.
(define (compound-native-type? type)
  (or (is-a? type <c-pointer>)
      (is-a? type <c-array>)
      (is-a? type <c-struct>)
      (is-a? type <c-union>)
      (is-a? type <c-function>)))

;;; Convert a <native-type> to a C type string.
(define (native-type->c-type type)
  (assume-type type <native-type>)
  (cond
    [(is-a? type <c-pointer>)
     (string-append (native-type->c-type (~ type'pointee-type)) "*")]
    [(is-a? type <c-array>)
     (string-append (native-type->c-type (~ type'element-type)) "*")]
    [(is-a? type <c-struct>)
     (if (~ type'tag) (format "struct ~a" (~ type'tag)) "struct /*anon*/")]
    [(is-a? type <c-union>)
     (if (~ type'tag) (format "union ~a" (~ type'tag)) "union /*anon*/")]
    [(is-a? type <c-function>)
     "void*"]                         ; function pointers used as opaque void*
    [else
     (~ type'c-type-name)]))

;;; Generate a C expression that unboxes a Scheme value to a C value.
;;; arg-expr: C string expression yielding ScmObj
(define (native-type->unbox-expr type arg-expr)
  (assume-type type <native-type>)
  (cond
    [(equal? (~ type'c-type-name) "void")
     (errorf "void cannot be used as an argument type")]
    [(compound-native-type? type)
     ;; Pointer and aggregate types are passed as native handles
     (format "(~a)ffi_unbox_ptr(~a)" (native-type->c-type type) arg-expr)]
    [else
     (let1 unboxer (~ type'c-unboxer-name)
       (if (or (not unboxer) (equal? unboxer ""))
         (errorf "No unboxer for type: ~s" type)
         (format "~a(~a)" unboxer arg-expr)))]))

;;; Generate a C expression that boxes a C value back to a Scheme object.
;;; val-expr: C string expression yielding the C value.
(define (native-type->box-expr type val-expr)
  (assume-type type <native-type>)
  (cond
    [(equal? (~ type'c-type-name) "void")
     (format "SCM_VOID_RETURN_VALUE(~a)" val-expr)]
    [(compound-native-type? type)
     (format "ffi_box_ptr(~a)" val-expr)]
    [else
     (let1 boxer (~ type'c-boxer-name)
       (if (or (not boxer) (equal? boxer ""))
         (errorf "No boxer for type: ~s" type)
         (format "~a(~a)" boxer val-expr)))]))

;;;
;;; C code emitters
;;; Each emitter takes a <foreign-c-function> instance.
;;;

;;; Emit function pointer variable (decl section).
;;; e.g.: static int (*ffi_fn_mylib_add)(int, int) = NULL;
(define (emit-fn-ptr-decl cfn)
  (let* ([c-name    (~ cfn'c-name)]
         [arg-types (~ cfn'arg-types)]
         [ret-type  (~ cfn'return-type)]
         [ret-c     (native-type->c-type ret-type)]
         [arg-c-str (if (null? arg-types)
                      "void"
                      (string-join (map native-type->c-type arg-types) ", "))])
    (cgen-decl (format "static ~a (*ffi_fn_~a)(~a) = NULL;"
                       ret-c c-name arg-c-str))))

;;; Emit the SUBR function for one FFI function (body section).
(define (emit-subr-body cfn)
  (let* ([scm-name  (~ cfn'scheme-name)]
         [c-name    (~ cfn'c-name)]
         [arg-types (~ cfn'arg-types)]
         [ret-type  (~ cfn'return-type)])
    (cgen-body
     (format "static ScmObj ffi_subr_~a(ScmObj *args, int nargs SCM_UNUSED, void *data SCM_UNUSED)"
             c-name))
    (cgen-body "{")
    ;; Guard: function pointer must have been set up
    (cgen-body (format "    if (ffi_fn_~a == NULL)" c-name))
    (cgen-body (format "        Scm_Error(\"FFI: ~a is not initialized; call with-ffi first\");"
                       (symbol->string scm-name)))
    ;; Unbox each argument into a typed C local variable
    (for-each-with-index
     (lambda (i arg-type)
       (let* ([c-type (native-type->c-type arg-type)]
              [unbox  (native-type->unbox-expr arg-type (format "args[~a]" i))])
         (cgen-body (format "    ~a arg~a = ~a;" c-type i unbox))))
     arg-types)
    ;; Build the call expression and box the result
    (let1 call-expr
        (format "ffi_fn_~a(~a)"
                c-name
                (string-join (map (^i (format "arg~a" i))
                                  (iota (length arg-types)))
                             ", "))
      (cgen-body (format "    return ~a;"
                         (native-type->box-expr ret-type call-expr))))
    (cgen-body "}")))

;;; Return a string with the setup code for one FFI function.
;;; This is emitted inside Scm_FFISetup_UNIT which takes a ScmDLObj*.
(define (setup-code-for-fn cfn)
  (let* ([scm-name  (~ cfn'scheme-name)]
         [c-name    (~ cfn'c-name)])
    (string-append
     (format "    fptr = Scm_DLOGetEntryAddress(dlo, SCM_STRING(SCM_MAKE_STR(~a)));"
             (cgen-safe-string c-name))
     "\n"
     (format "    if (SCM_FALSEP(fptr))\
            \n        Scm_Error(\"FFI setup: symbol ~a not found in library\");"
             c-name)
     "\n"
     (format "    *(void**)&ffi_fn_~a = SCM_FOREIGN_POINTER_REF(void*, fptr);"
             c-name))))

;;; Return a string with the init code to bind one SUBR as a Scheme procedure.
;;; This is emitted inside Scm_Init_UNIT.
(define (init-code-for-fn cfn)
  (let* ([scm-name (~ cfn'scheme-name)]
         [c-name   (~ cfn'c-name)]
         [nargs    (length (~ cfn'arg-types))])
    (format "    Scm_Define(SCM_CURRENT_MODULE(), SCM_SYMBOL(SCM_INTERN(~a)),\
           \n    Scm_MakeSubr(ffi_subr_~a, NULL, ~a, 0, SCM_INTERN(~a)));"
            (cgen-safe-string (symbol->string scm-name))
            c-name
            nargs
            (cgen-safe-string (symbol->string scm-name)))))

;;;
;;; Generate C code from a list of <foreign-c-function> instances.
;;;

(define (generate-ffi-c-code-unit cfn-instances)
  (define unit
    (make <cgen-unit>
      :name "ffi"
      :preamble
      (list "/* Generated by gauche.ffi.stubgen.  Do not edit. */")
      :init-name "ffi"))
  (parameterize ([cgen-current-unit unit])
    (cgen-decl "#include <gauche.h>")

    (for-each emit-fn-ptr-decl cfn-instances)

    (cgen-body "")
    (for-each emit-subr-body cfn-instances)

    ;; FFI setup function (body section) — called by with-ffi
    (cgen-body ""
               "ScmObj ffisetup(ScmObj *argv, int argc, void *data)"
               "{"
               "    SCM_ASSERT(argc == 1);"
               "    SCM_ASSERT(SCM_DLOBJP(argv[0]));"
               "    ScmDLObj *dlo = SCM_DLOBJ(argv[0]);"
               "    ScmObj fptr;")
    (dolist [cfn cfn-instances]
      (cgen-body (setup-code-for-fn cfn)))
    (cgen-body "    return SCM_UNDEFINED;"
               "}")

    ;; Scheme procedure bindings (init section, inside Scm_Init_UNIT)
    (dolist [cfn cfn-instances]
      (cgen-init (init-code-for-fn cfn)))

    (cgen-init "    Scm_Define(SCM_CURRENT_MODULE(),"
               "               SCM_SYMBOL(SCM_INTERN(\"ffisetup\")),"
               "               Scm_MakeSubr(ffisetup, NULL, 1, 0, SCM_FALSE));")
    )
  ;; Return unit
  unit)
