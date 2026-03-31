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
       [(_ dlo-expr options cfn-forms body)
        (quasirename r
          `(begin
             (define _dummy
               (compile-and-link-ffi-stub ,dlo-expr
                                          ',cfn-forms
                                          (current-module)))
             ,@body))]))))

(define (compile-and-link-ffi-stub dlobj cfn-forms mod)
  (let ([unit (generate-ffi-c-code-unit cfn-forms)])
    (cgen-dynamic-load unit (sys-tmpdir))
    ((module-binding-ref mod 'ffisetup) dlobj)))

;;;
;;; Type resolution via <native-type>
;;;
;;; Type specifications accepted here:
;;;   - Symbols like 'int, 'double, 'c-string, 'void, 'int*, etc.
;;;   - S-expressions like (.array int (3)), (.struct tag (...)), etc.
;;;   - Already-resolved <native-type> instances (passed via unquoting)
;;;
;;; All of these are handled by (native-type sig) from gauche.ctype,
;;; which returns a <native-type> instance whose slots c-type-name,
;;; c-boxer-name, and c-unboxer-name carry the C code fragments.
;;;

;;; True if a native type is compound (pointer, array, struct, union,
;;; or function pointer).  Compound types are passed as native handles.
(define (compound-native-type? type)
  (or (is-a? type <c-pointer>)
      (is-a? type <c-array>)
      (is-a? type <c-struct>)
      (is-a? type <c-union>)
      (is-a? type <c-function>)))

;;; Convert a type specification to a C type string.
;;; Handles scalars, pointer chains, and compound types.
(define (type-sig->c-type sig)
  (let1 type (native-type sig)
    (cond
      [(is-a? type <c-pointer>)
       (string-append (type-sig->c-type (~ type'pointee-type)) "*")]
      [(is-a? type <c-array>)
       (string-append (type-sig->c-type (~ type'element-type)) "*")]
      [(is-a? type <c-struct>)
       (if (~ type'tag) (format "struct ~a" (~ type'tag)) "struct /*anon*/")]
      [(is-a? type <c-union>)
       (if (~ type'tag) (format "union ~a" (~ type'tag)) "union /*anon*/")]
      [(is-a? type <c-function>)
       "void*"]                         ; function pointers used as opaque void*
      [else
       (~ type'c-type-name)])))

;;; Generate a C expression that unboxes a Scheme value to a C value.
;;; arg-expr: C string expression yielding ScmObj
(define (type-sig->unbox-expr sig arg-expr)
  (let1 type (native-type sig)
    (cond
      [(equal? (~ type'c-type-name) "void")
       (errorf "void cannot be used as an argument type")]
      [(compound-native-type? type)
       ;; Pointer and aggregate types are passed as native handles
       (format "(~a)ffi_unbox_ptr(~a)" (type-sig->c-type sig) arg-expr)]
      [else
       (let1 unboxer (~ type'c-unboxer-name)
         (if (or (not unboxer) (equal? unboxer ""))
           (errorf "No unboxer for type: ~s" sig)
           (format "~a(~a)" unboxer arg-expr)))])))

;;; Generate a C expression that boxes a C value back to a Scheme object.
;;; val-expr: C string expression yielding the C value.
;;; For void return type, val-expr is the C call expression itself;
;;; SCM_VOID_RETURN_VALUE evaluates it (discarding the void result)
;;; and yields SCM_UNDEFINED.
(define (type-sig->box-expr sig val-expr)
  (let1 type (native-type sig)
    (cond
      [(equal? (~ type'c-type-name) "void")
       (format "SCM_VOID_RETURN_VALUE(~a)" val-expr)]
      [(compound-native-type? type)
       (format "ffi_box_ptr(~a)" val-expr)]
      [else
       (let1 boxer (~ type'c-boxer-name)
         (if (or (not boxer) (equal? boxer ""))
           (errorf "No boxer for type: ~s" sig)
           (format "~a(~a)" boxer val-expr)))])))

;;;
;;; Parsing define-c-function
;;;

;;; Parse the return type specification from the tail of a define-c-function form.
;;; rest is the portion of the form after the arglist, so:
;;;   (::int)              for  (define-c-function name arglist ::int)
;;;   (:: (.array int (3))) for  (define-c-function name arglist :: (.array int (3)))
;;; Returns the type signature (symbol or S-expr).
(define (parse-return-type rest)
  (match rest
    ;; Simple case: single ::type keyword, e.g. ::int
    ;; In Gauche, ::int is a keyword whose keyword->string returns ":int"
    [(kw)
     (unless (keyword? kw)
       (errorf "Expected return type ::TYPE keyword, got: ~s" kw))
     (let1 s (keyword->string kw)
       (unless (#/^:/ s)
         (errorf "Expected ::TYPE, got :~a (did you mean ::~a?)" s s))
       (string->symbol (string-copy s 1)))]
    ;; Compound case: :: keyword followed by S-expr type
    ;; In Gauche, standalone :: is a keyword whose keyword->string returns ":"
    [(sep compound-type)
     (unless (and (keyword? sep) (equal? (keyword->string sep) ":"))
       (errorf "Expected :: separator before compound return type, got: ~s" sep))
     compound-type]
    [_
     (errorf "Invalid return type specification: ~s" rest)]))

;;; Parse a single define-c-function form.
;;; Returns a plist:
;;;   :scheme-name  original Scheme name symbol (e.g., mylib-add)
;;;   :c-name       C-safe identifier string (e.g., "mylib_add")
;;;   :nargs        number of required arguments
;;;   :arg-specs    list of (c-arg-name type-sig) pairs
;;;   :rettype      return type signature (symbol or S-expr)
(define (parse-define-c-function form)
  (match form
    [('define-c-function name arglist . rettype-rest)
     (unless (symbol? name)
       (errorf "define-c-function: name must be a symbol, got: ~s" name))
     (unless (list? arglist)
       (errorf "define-c-function: arglist must be a list, got: ~s" arglist))
     (let* ([canonical-args (cgen-canonical-typed-var-list arglist 'ScmObj)]
            [rettype (parse-return-type rettype-rest)]
            [arg-specs
             (map-with-index
              (^[i ca]
                (match ca
                  [(aname ':: atype . _)
                   ;; Derive a C argument name from the Scheme arg name, or
                   ;; fall back to "argN" for unnamed or default-typed args.
                   (let1 c-argname
                       (if (and aname (not (eq? aname 'ScmObj)))
                         (cgen-safe-name-friendly (symbol->string aname))
                         (format "arg~a" i))
                     (list c-argname atype))]
                  [_ (errorf "Unexpected canonical arg form: ~s" ca)]))
              canonical-args)]
            [c-name (cgen-safe-name-friendly (symbol->string name))])
       `(define-c-function-info
          :scheme-name ,name
          :c-name ,c-name
          :nargs ,(length arg-specs)
          :arg-specs ,arg-specs
          :rettype ,rettype))]
    [_
     (errorf "Invalid define-c-function form: ~s" form)]))

;;; Utility: retrieve a value from the info plist by keyword.
(define (info-ref info key)
  (get-keyword key (cdr info)))

;;;
;;; C code emitters
;;;

;;; Emit function pointer variable (decl section).
;;; e.g.: static int (*ffi_fn_mylib_add)(int, int) = NULL;
(define (emit-fn-ptr-decl info)
  (let* ([c-name    (info-ref info :c-name)]
         [arg-specs (info-ref info :arg-specs)]
         [rettype   (info-ref info :rettype)]
         [ret-c     (type-sig->c-type rettype)]
         [arg-c-str (if (null? arg-specs)
                      "void"
                      (string-join (map (compose type-sig->c-type cadr) arg-specs) ", "))])
    (cgen-decl (format "static ~a (*ffi_fn_~a)(~a) = NULL;"
                       ret-c c-name arg-c-str))))

;;; Emit the SUBR function for one FFI function (body section).
(define (emit-subr-body info)
  (let* ([scm-name  (info-ref info :scheme-name)]
         [c-name    (info-ref info :c-name)]
         [arg-specs (info-ref info :arg-specs)]
         [rettype   (info-ref info :rettype)])
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
     (lambda (i spec)
       (let* ([arg-c-name (car spec)]
              [type-sig   (cadr spec)]
              [c-type     (type-sig->c-type type-sig)]
              [unbox      (type-sig->unbox-expr type-sig (format "args[~a]" i))])
         (cgen-body (format "    ~a c_~a = ~a;" c-type arg-c-name unbox))))
     arg-specs)
    ;; Build the call expression and box the result
    (let1 call-expr
        (format "ffi_fn_~a(~a)"
                c-name
                (string-join (map (lambda (s) (string-append "c_" (car s)))
                                  arg-specs)
                             ", "))
      (cgen-body (format "    return ~a;"
                         (type-sig->box-expr rettype call-expr))))
    (cgen-body "}")))

;;; Return a string with the setup code for one FFI function.
;;; This is emitted inside Scm_FFISetup_UNIT which takes a ScmDLObj*.
(define (setup-code-for-fn info)
  (let* ([scm-name  (info-ref info :scheme-name)]
         [c-name    (info-ref info :c-name)])
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
(define (init-code-for-fn info)
  (let* ([scm-name (info-ref info :scheme-name)]
         [c-name   (info-ref info :c-name)]
         [nargs    (info-ref info :nargs)])
    (format "    Scm_Define(SCM_CURRENT_MODULE(), SCM_SYMBOL(SCM_INTERN(~a)),\
           \n    Scm_MakeSubr(ffi_subr_~a, NULL, ~a, 0, SCM_INTERN(~a)));"
            (cgen-safe-string (symbol->string scm-name))
            c-name
            nargs
            (cgen-safe-string (symbol->string scm-name)))))

;;;
;;; Generate C code from a list of define-c-function forms.
;;;

(define (generate-ffi-c-code-unit forms)
  (define unit
    (make <cgen-unit>
      :name "ffi"
      :preamble
      (list "/* Generated by gauche.ffi.stubgen.  Do not edit. */")
      :init-name "ffi"))
  (parameterize ([cgen-current-unit unit])
    (cgen-decl "#include <gauche.h>")

    ;; Parse all forms
    (let1 all-infos (map parse-define-c-function forms)
      (for-each emit-fn-ptr-decl all-infos)

      (cgen-body "")
      (for-each emit-subr-body all-infos)

      ;; FFI setup function (body section) — called by with-ffi
      (cgen-body ""
                 "ScmObj ffisetup(ScmObj *argv, int argc, void *data)"
                 "{"
                 "    SCM_ASSERT(argc == 1);"
                 "    SCM_ASSERT(SCM_DLOBJP(argv[0]));"
                 "    ScmDLObj *dlo = SCM_DLOBJ(argv[0]);"
                 "    ScmObj fptr;")
      (dolist [info all-infos]
        (cgen-body (setup-code-for-fn info)))
      (cgen-body "    return SCM_UNDEFINED;"
                 "}")

      ;; Scheme procedure bindings (init section, inside Scm_Init_UNIT)
      (dolist [info all-infos]
        (cgen-init (init-code-for-fn info))))

    (cgen-init "    Scm_Define(SCM_CURRENT_MODULE(),"
               "               SCM_SYMBOL(SCM_INTERN(\"ffisetup\")),"
               "               Scm_MakeSubr(ffisetup, NULL, 1, 0, SCM_FALSE));")
    )
    ;; Return unit
  unit)
