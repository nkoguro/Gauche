;;;
;;; gauche.ffi - Foreign function interfce
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
  (export with-ffi
          define-c-function)
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
;;  an empty list.
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
;;  <arglist> and <rettype> are slimiar to CiSE function, e.g.
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
;;
;;  with-ffi form works in 2 passes.
;;
;;  The first pass collects define-c-function, and generate C code in a
;;  temporary file that does the following:
;;
;;    - Define a C pointer variable where looked up function pointer
;;      is set.
;;    - Define a SUBR that accepts Scheme argments, type checks and
;;      converts them, then call the C function pointer.  Receives the
;;      result and converts it back to Scheme to return.
;;      It keeps a pointer ScmCFunction in 'data' slot, which is used
;;      for type checking and conversion.
;;    - An FFI setup function that takes ScmDLObj.  It sets the C
;;      function pointers needed in each SUBR.
;;    - An initializatoin function Scm_Init.xxx that takes ScmModule.
;;      In it, bindings for the Scheme procedure with the generated SUBR,
;;      using Scm_MakeSubr.
;;
;;  Then it calls C compiler and linker on the generated source to produce
;;  DSO.
;;
;;  The second pass generates a Scheme code (as a macro expander) that does
;;  the following:
;;
;;    - Evaluate <dlobj> expression to obtain #<dlobj> for external DSO.
;;    - Load teh generated DSO in the first pass.
;;    - Call FFI setup function with the extenal DSO.
;;    - Emit rest of the code excluding define-c-function.

(define ffi-dlo (make-parameter #f))

(define-syntax define-c-function
  (syntax-rules ()
    [(_ . _)
     (syntax-error "define-c-function used outside with-ffi")]))

(autoload gauche.ffi.stubgen (:macro with-stubgen-ffi))

(define-syntax with-ffi
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ dlo-expr options . body)
        (define cfns '())
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
        (quasirename r
          (with-stubgen-ffi ,dlo-expr ,options ,(reverse cfns) ,forms))]))))
