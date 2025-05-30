;;;
;;; libmisc.scm - miscellaneous built-in procedures
;;;
;;;   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)
(inline-stub
 (.include "gauche/priv/configP.h"
           "gauche/vminsn.h"))

;;;
;;; Miscellaneous
;;;

(select-module gauche)

(define-cproc has-setter? (proc) ::<boolean> Scm_HasSetter)

(define-cproc undefined () (inliner CONSTU) (return SCM_UNDEFINED))
(define-cproc undefined? (obj) ::<boolean> :constant SCM_UNDEFINEDP)

(define (warn fmt . args)
  (unless (sys-getenv "GAUCHE_SUPPRESS_WARNING")
    (apply format (current-error-port) (string-append "WARNING: " fmt) args)
    (flush (current-error-port))))

;; uninitialized value is only used in the compiler and should never be leaked
;; out to the userland.
(select-module gauche.internal)
(define-cproc %uninitialized () (return SCM_UNINITIALIZED))

;; Debug label
(select-module gauche)
(define-cproc debug-label (obj) (return (Scm_Sprintf "@%lx" obj)))

;; Foreign pointer (may be in libsys.scm?)

(select-module gauche)

(define-cproc foreign-pointer-invalid? (fp::<foreign-pointer>) ::<boolean>
  Scm_ForeignPointerInvalidP)

(define-cproc foreign-pointer-invalidate! (fp::<foreign-pointer>) ::<void>
  Scm_ForeignPointerInvalidate)

(define-cproc foreign-pointer-attributes (fp::<foreign-pointer>)
  Scm_ForeignPointerAttr)

(define-cproc foreign-pointer-attribute-get (fp::<foreign-pointer>
                                             key :optional fallback)
  Scm_ForeignPointerAttrGet)

(define-cproc foreign-pointer-attribute-set! (fp::<foreign-pointer> key value)
  Scm_ForeignPointerAttrSet)

; for backward compatibility - deprecated
(define foreign-pointer-attribute-set foreign-pointer-attribute-set!)

;;
;; Static configuration
;;

;; We intentionally hide runtime access to the cond-feature identifiers.
;; Cond-features work at compile time, and manipulating them at runtime
;; may lead confusing behavior.  The set of feature identifiers should
;; be fixed before compiling/loading any external libraries.
(select-module gauche.internal)
(define-cproc cond-features () Scm_GetFeatures)
(define-cproc add-cond-feature! (feature::<symbol>
                                 :optional (module::<symbol>? #f))
  ::<void>
  (let* ([cfeature::(const char*)
                    (Scm_GetStringConst (SCM_SYMBOL_NAME feature))]
         [cmod::(const char*)
                (?: module
                    (Scm_GetStringConst (SCM_SYMBOL_NAME module))
                    NULL)])
  (Scm_AddFeature cfeature cmod)))
(define-cproc delete-cond-feature! (feature::<symbol>)
  ::<void>
  (Scm_DeleteFeature (Scm_GetStringConst (SCM_SYMBOL_NAME feature))))

(inline-stub
 (define-constant SLIB_DIR (c (SCM_MAKE_STR_IMMUTABLE SLIB_DIR))))
