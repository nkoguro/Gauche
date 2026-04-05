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
          <foreign-c-function>)
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
;;  <arglist> and <rettype> are evaluated expressions.  <arglist> must yield
;;  a list of typespecs, and <rettype> must yield a typespec.  A typespec is
;;  either a native-type signature symbol/S-expr (resolved via native-type)
;;  or a <native-type> instance directly.
;;
;;   (define-c-function mylib-init '(int (.array c-string)) 'int)
;;
;;   (define-c-function mylib-init `(,<c-int> ,(make-c-array-type <c-string>)) <c-int>)
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

;; Resolve a typespec to a <native-type> instance at runtime.
;; Reference to this procedure is inserted by macro expander.
;; A typespec is either a <native-type> instance (returned as-is) or a
;; native-type signature.
(define (%resolve-typespec spec)
  (if (is-a? spec <native-type>)
    spec
    (native-type spec)))

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
        ;; For each define-c-function form, build a runtime
        ;; (make <foreign-c-function> ...) expression.
        (define (make-cfn-expr cfn-form)
          (match cfn-form
            [(_ name arg-types-expr rettype-expr)
             (quasirename r
               `(make <foreign-c-function>
                  :scheme-name ',name
                  :c-name ,(cgen-safe-name-friendly (x->string name))
                  :arg-types (map %resolve-typespec ,arg-types-expr)
                  :return-type (%resolve-typespec ,rettype-expr)))]))
        ;; cfn-specs is ((name . cfn-expr) ...), where name is a symbol
        ;; name of cfn, and cfn-expr is (make <foreivn-c-function> ...)
        ;; constructed above.  The subsystem macro should rearrange
        ;; cfn-specs so that cfn-expr is evaluated in proper context.
        (let* ([ordered-cfns  (reverse cfns)]
               [cfn-specs     (map (^[cfn]
                                     (cons (cadr cfn) ; name
                                           (make-cfn-expr cfn))) ;expr
                                   ordered-cfns)])
          (ecase subsystem
            [(:stubgen)
             (quasirename r
               `(with-stubgen-ffi ,dlo-expr ,options ,cfn-specs ,forms))]
            [(:native)
             (quasirename r
               `(with-native-ffi ,dlo-expr ,options ,cfn-specs ,forms))]
            ))]))))
