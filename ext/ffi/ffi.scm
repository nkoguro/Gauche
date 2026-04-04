;;;
;;; gauche.ffi - Foreign function interface
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

(define-module gauche.ffi
  (use util.match)
  (use gauche.ctype)
  (use gauche.cgen.unit :only (cgen-safe-name-friendly))
  (export with-ffi
          define-c-function
          <foreign-c-function>
          parse-define-c-function)
  )
(select-module gauche.ffi)

;; FFI syntax
;;
;;  We'll have multiple FFI backends, but this high-level module
;;  hides the underlying implementation.
;;
;;  All FFI definitions must be enclosed by `with-ffi` form, which sets
;;  up the enviornment to define FFI functions
;;
;;   (with-ffi <dlobj> (<option> ...)
;;     <body> ...)
;;
;;  <dlobj> is an expression that yields #<dlobj>, e.g. call to 'dynamic-load'.
;;
;;  The list of <option>s is for future extension.  Currently it should be
;;  an empty list.  (:subsystem :native) selects the native FFI backend.
;;
;;  What follows are <body> just like let body.  In it, you can use
;;  define-c-function form at the toplevel.
;;
;;  Foreign function can be defined as follows:
;;
;;   (define-c-function <name>
;;     <arglist> <rettype>)
;;
;;  <name> is an identifier that must match the exported function name
;;  in the <dlobj>.
;;
;;  <arglist> and <rettype> are similar to CiSE function, e.g.
;;
;;   (define-c-function mylib-init
;;     (argc::int argv::(.array c-string)) ::int)
;;
;;  Or, you can include an expression that yields <native-type> by unquoting
;;  it, e.g.
;;
;;   (define-c-function mylib-init
;;     (argc::,<c-int> argv::,(make-c-array-type <c-string>)) ::,<c-int>)
;;
;;  The argument names are only for information.  They can be omitted.
;;
;;   (define-c-function mylib-init
;;     (::int ::(.array c-string)) ::int)

;;;
;;; <foreign-c-function> - parsed representation of a define-c-function form
;;;
;;; Created by parse-define-c-function at macro-expansion time.
;;; Backend macros receive a list of its instances.
;;;

(define-class <foreign-c-function> ()
  ((scheme-name  :init-keyword :scheme-name)  ; symbol
   (c-name       :init-keyword :c-name)       ; string, C-safe function name
   (args         :init-keyword :args)         ; list of (symbol <native-type>)
   (return-type  :init-keyword :return-type)  ; <native-type>
   ))

;;;
;;; Parsing helpers
;;;

;; Resolve a type spec to a <native-type> instance.
;;   <native-type> already   - returned as-is
;;   (unquote expr)          - eval expr in gauche module (covers all builtins)
;;   symbol like 'int        - resolved via native-type
;;   S-expr like (.array ...) - resolved via native-type
(define (%resolve-type-spec spec)
  (match spec
    [(? (cut is-a? <> <native-type>)) spec]
    [('unquote expr) (eval expr (find-module 'gauche))]
    [_ (native-type spec)]))

;; Parse the return-type tail of define-c-function.
;; rettype-rest is the list after the arglist:
;;   (::int)               - keyword ::int
;;   (:: (.array int (3))) - separator :: then compound type
;;   (:: ,<c-int>)         - separator :: then unquoted expression
;; Returns the raw type spec (symbol, S-expr, or (unquote expr)).
(define (%parse-rettype-syntax rettype-rest)
  (match rettype-rest
    [(kw)
     (unless (keyword? kw)
       (errorf "Expected return type ::TYPE keyword, got: ~s" kw))
     (let1 s (keyword->string kw)
       (unless (#/^:/ s)
         (errorf "Expected ::TYPE, got :~a (did you mean ::~a?)" s s))
       (string->symbol (string-copy s 1)))]
    [(sep compound-type)
     (unless (and (keyword? sep) (equal? ":" (keyword->string sep)))
       (errorf "Expected :: separator before compound return type, got: ~s" sep))
     compound-type]
    [_
     (errorf "Invalid return type specification: ~s" rettype-rest)]))

;; Parse a define-c-function arglist into a list of (arg-sym type-spec) pairs.
;; arg-sym is always a symbol (generated "argN" for unnamed args).
;; type-spec is a symbol, S-expression, or (unquote expr) form.
;;
;; Handles:
;;   name::type           - "name::type" as a single symbol
;;   name:: type          - "name::" symbol followed by type
;;   name :: type         - plain name, :: keyword separator, then type
;;   name ::type          - plain name, ::type keyword
;;   ::type               - unnamed: ::type keyword
;;   :: type              - unnamed: :: separator keyword followed by type
(define (%parse-cfn-arglist arglist)
  ;; Split "name::type" or "name::" into (name type-or-#f)
  (define (split-name::type sym)
    (and (symbol? sym)
         (let1 s (symbol->string sym)
           (cond
             [(#/^([^:]+)::([^:]+)$/ s)
              => (^m (list (string->symbol (m 1)) (string->symbol (m 2))))]
             [(#/^([^:]+)::$/ s)
              => (^m (list (string->symbol (m 1)) #f))]
             [else #f]))))
  ;; Extract type symbol from ::type keyword (keyword->string gives ":type")
  (define (keyword->type-sym kw)
    (and (keyword? kw)
         (let1 s (keyword->string kw)
           (cond
             [(#/^:([^:]+)$/ s) => (^m (string->symbol (m 1)))]
             [else #f]))))
  ;; True for the standalone "::" separator keyword
  (define (double-colon? x)
    (and (keyword? x) (equal? ":" (keyword->string x))))

  (let loop ([args arglist] [i 0] [result '()])
    (match args
      [()
       (reverse result)]
      ;; Keyword head checked BEFORE symbol: in Gauche, keywords are also
      ;; symbols, so we must distinguish ::type keywords from plain names.
      [((? keyword? kw) . rest)
       (cond
         [(keyword->type-sym kw)
          ;; ::type keyword - unnamed arg
          => (^t
               (let1 sym (string->symbol (format "arg~a" i))
                 (loop rest (+ i 1) (cons (list sym t) result))))]
         [(double-colon? kw)
          ;; :: separator keyword - unnamed arg, type follows
          (match rest
            [(type . rest2)
             (let1 sym (string->symbol (format "arg~a" i))
               (loop rest2 (+ i 1) (cons (list sym type) result)))]
            [_ (error "missing type after ::")])]
         [else
          (loop rest i result)])]
      ;; Non-keyword symbol
      [((? symbol? head) . rest)
       (cond
         [(split-name::type head)
          => (^[nt]
               (if (cadr nt)
                 ;; name::type embedded
                 (loop rest (+ i 1) (cons nt result))
                 ;; name:: - type follows
                 (match rest
                   [(type . rest2)
                    (loop rest2 (+ i 1) (cons (list (car nt) type) result))]
                   [_ (error "missing type after" head)])))]
         [else
          ;; Plain name - look for :: separator or ::type keyword
          (match rest
            [((? double-colon?) type . rest2)
             (loop rest2 (+ i 1) (cons (list head type) result))]
            [((? keyword? kw) . rest2)
             (let1 t (keyword->type-sym kw)
               (if t
                 (loop rest2 (+ i 1) (cons (list head t) result))
                 (loop rest i result)))]
            [_
             (loop rest (+ i 1) (cons (list head 'void) result))])])]
      ;; Anything else - skip
      [(_ . rest)
       (loop rest i result)])))

;;;
;;; Public API
;;;

;; Parse a (define-c-function name arglist . rettype-rest) form into a
;; <foreign-c-function> instance, resolving all type specifications to
;; <native-type> instances immediately.
;;
;; This is called at macro-expansion time inside with-ffi's transformer.
;; The resulting instances are live Scheme objects passed as literal data
;; to the backend macros.
(define (parse-define-c-function form)
  (match form
    [('define-c-function name arglist . rettype-rest)
     (unless (symbol? name)
       (errorf "define-c-function: name must be a symbol, got: ~s" name))
     (let* ([sym-specs   (%parse-cfn-arglist (unwrap-syntax arglist))]
            [args         (map (^[ss]
                                 (list (car ss)
                                       (%resolve-type-spec (cadr ss))))
                               sym-specs)]
            [ret-spec     (%parse-rettype-syntax rettype-rest)]
            [return-type  (%resolve-type-spec ret-spec)])
       (make <foreign-c-function>
         :scheme-name name
         :c-name      (cgen-safe-name-friendly (x->string name))
         :args        args
         :return-type return-type))]
    [_
     (errorf "Invalid define-c-function form: ~s" form)]))

;;;
;;; Syntax
;;;

(define-syntax define-c-function
  (syntax-rules ()
    [(_ . _)
     (syntax-error "define-c-function used outside with-ffi")]))

(autoload gauche.ffi.stubgen (:macro with-stubgen-ffi))
(autoload gauche.ffi.native  (:macro with-native-ffi))

(define-syntax with-ffi
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ dlo-expr options . body)
        (define cfns '())
        (define subsystem
          (get-keyword :subsystem (unwrap-syntax options) :stubgen))
        (define forms
          (filter-map
           (^[form]
             (if (and (pair? form)
                      (c (r (car form)) (r 'define-c-function))
                      (pair? (cdr form)))
               (begin
                 (push! cfns (unwrap-syntax form))
                 #f)
               form))
           body))
        ;; Parse all collected define-c-function forms into
        ;; <foreign-c-function> instances at macro-expansion time.
        ;; These live objects are embedded directly into the generated form.
        (let1 cfn-instances (map parse-define-c-function (reverse cfns))
          (ecase subsystem
            [(:stubgen)
             (quasirename r
               `(with-stubgen-ffi ,dlo-expr ,options ,cfn-instances ,forms))]
            [(:native)
             (quasirename r
               `(with-native-ffi ,dlo-expr ,options ,cfn-instances ,forms))]))]))))
