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
;;  <arglist> :: (<typespec> ...)
;;  <retttype> :: <typespec>
;;
;;  <typespec> can be either native type signature, or ,type-expr
;;  where type-expr must yield <native-type>.
;;
;;   (define-c-function mylib-init
;;     (int (.array c-string)) int)
;;
;;   (define-c-function mylib-init
;;     (,<c-int> ,(make-c-array-type <c-string>)) ,<c-int>)
;;

;;;
;;; <foreign-c-function> - parsed representation of a define-c-function form
;;;
;;; Created by parse-define-c-function at macro-expansion time.
;;; Backend macros receive a list of its instances.
;;;

(define-class <foreign-c-function> ()
  ((scheme-name  :init-keyword :scheme-name)  ; symbol
   (c-name       :init-keyword :c-name)       ; string, C-safe function name
   (arg-types    :init-keyword :arg-types)    ; list of <native-type>
   (return-type  :init-keyword :return-type)  ; <native-type>
   ))

;;;
;;; Parsing helpers
;;;

;; Resolve a type spec to a <native-type> instance.
;;   <native-type> already    - returned as-is
;;   (unquote expr)           - eval expr in gauche module (covers all builtins)
;;   symbol like 'int         - resolved via native-type
;;   S-expr like (.array ...) - resolved via native-type
(define (%resolve-type-spec spec)
  (match spec
    [(? (cut is-a? <> <native-type>)) spec]
    [('unquote expr) (eval expr (find-module 'gauche))]
    [_ (native-type spec)]))

;;;
;;; Public API
;;;

;; Parse a (define-c-function name (typespec ...) rettype) form into a
;; <foreign-c-function> instance, resolving all type specifications to
;; <native-type> instances immediately.
;;
;; This is called at macro-expansion time inside with-ffi's transformer.
;; The resulting instances are live Scheme objects passed as literal data
;; to the backend macros.
(define (parse-define-c-function form)
  (match form
    [('define-c-function name arglist rettype)
     (unless (symbol? name)
       (errorf "define-c-function: name must be a symbol, got: ~s" name))
     (let* ([type-specs  (unwrap-syntax arglist)]
            [arg-types   (map %resolve-type-spec type-specs)]
            [return-type (%resolve-type-spec rettype)])
       (make <foreign-c-function>
         :scheme-name name
         :c-name      (cgen-safe-name-friendly (x->string name))
         :arg-types   arg-types
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
