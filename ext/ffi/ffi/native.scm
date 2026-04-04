;;;
;;; gauche.ffi.native - Native FFI interface
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

;; Experimental.  Only available on selected platforms (OS/CPU)

(define-module gauche.ffi.native
  (use gauche.ffi)
  (use gauche.ctype)
  (use util.match)
  (export with-native-ffi))
(select-module gauche.ffi.native)

;;;
;;; Main macro
;;;

;; (with-native-ffi dlo-expr options cfn-instances body)
;;
;; cfn-instances is a list of <foreign-c-function> objects produced by
;; with-ffi at macro-expansion time.  For each entry, a Scheme procedure
;; is defined that calls the named C function via call-amd64.
(define-syntax with-native-ffi
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ dlo-expr options cfn-instances body)
        (let1 dlo-var (gensym "dlo")
          (quasirename r
            `(let1 ,dlo-var ,dlo-expr
               ,@(map (cut generate-c-function-entry <> r dlo-var) cfn-instances)
               ,@body)))]))))

;;;
;;; Type canonicalization
;;;

;; Convert a <native-type> instance to the call-amd64 canonical type symbol:
;;   'v  - void (return only)
;;   'd  - double (xmm register, 64-bit float)
;;   'f  - float  (xmm register, 32-bit float)
;;   's  - C string (Scheme string passed as const char*)
;;   'p  - raw pointer (native-handle or foreign-pointer)
;;   'i  - integer (all other integral types)
(define (native-type->call-canon type)
  (assume-type type <native-type>)
  (cond
    [(or (is-a? type <c-pointer>)
         (is-a? type <c-array>)
         (is-a? type <c-function>))  'p]
    [(eq? type <void>)               'v]
    [(eq? type <double>)             'd]
    [(eq? type <float>)              'f]
    [(eq? type <c-string>)           's]
    [else                            'i]))

;;;
;;; Code generation
;;;

;; Generate a (define name ...) form for one <foreign-c-function> instance.
;;
;; cfn     - <foreign-c-function> instance
;; r       - er-macro renamer
;; dlo-var - symbol bound to the dlobj in the generated let
;;
;; Canonical types are computed at macro-expansion time from the already-resolved
;; <native-type> instances and baked in as quoted literals.  The generated code:
;;  1. Looks up the function pointer from the dlobj once at definition time.
;;  2. Produces a lambda that builds the typed-arg list and invokes call-amd64.
(define (generate-c-function-entry cfn r dlo-var)
  (let* ([name       (~ cfn'scheme-name)]
         [c-name     (~ cfn'c-name)]
         [arg-types  (~ cfn'arg-types)] ; (<native-type> ...)
         [ret-type   (~ cfn'return-type)]
         [arg-syms   (map (^i (string->symbol (format "arg~a" i)))
                          (iota (length arg-types)))]
         [ret-canon  (native-type->call-canon ret-type)]
         [arg-canons (map native-type->call-canon arg-types)])
    (quasirename r
      `(define ,name
         (let1 ptr (dlobj-get-entry-address ,dlo-var ,c-name)
           (unless ptr
             (error "FFI (native): cannot find function in library:" ',name))
           (^ ,arg-syms
              ((with-module gauche.internal call-amd64)
               ptr
               (list ,@(map (^[sym canon] `(,(r 'list) ',canon ,sym))
                            arg-syms arg-canons))
               ',ret-canon)))))))
