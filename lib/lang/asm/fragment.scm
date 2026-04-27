;;;
;;; Named assembly fragment with build-time serialization
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

;; This module is used only at build time, in code-generation scripts such as
;; gen-native.scm.  It is not loaded at runtime.
;;
;; Usage pattern:
;;
;;   (define-asm-fragment foo x86_64
;;     '(entry: (movq (imm64 :val) %rax) ...))
;;
;;   (dump-asm-fragment foo port)
;;
;; define-asm-fragment assembles the source immediately and stores the result
;; (an <obj-template>) in the <asm-fragment> object bound to foo.
;;
;; dump-asm-fragment writes to port:
;;   - a #| ... |# block with the human-readable assembly listing
;;   - a (define foo-tmpl ...) form that, when evaluated in native-supp.scm,
;;     defines a zero-argument function foo-tmpl.  Calling (foo-tmpl) returns
;;     the deserialized <obj-template>, deserializing only on the first call
;;     and caching thereafter.  The deserializer is looked up via
;;     module-binding-ref because lang.asm.linker is not loaded when
;;     native-supp.scm initializes.

(define-module lang.asm.fragment
  (use lang.asm.linker)
  (use lang.asm.x86_64)
  (export <asm-fragment> make-asm-fragment asm-fragment?
          define-asm-fragment dump-asm-fragment))
(select-module lang.asm.fragment)

(define-class <asm-fragment> ()
  ((name     :init-keyword :name)     ; symbol
   (arch     :init-keyword :arch)     ; symbol, e.g. 'x86_64
   (src      :init-keyword :src)      ; list — the raw asm source
   (template :init-keyword :template) ; <obj-template>
   ))

(define (asm-fragment? x) (is-a? x <asm-fragment>))

(define (assemble-for-arch arch src)
  (case arch
    [(x86_64) (x86_64-asm src)]
    [else (errorf "make-asm-fragment: unsupported architecture: ~s" arch)]))

(define (listing-for-arch arch src)
  (case arch
    [(x86_64) (x86_64-dump src)]
    [else (errorf "dump-asm-fragment: unsupported architecture: ~s" arch)]))

(define (make-asm-fragment name arch src)
  (make <asm-fragment>
    :name name
    :arch arch
    :src  src
    :template (assemble-for-arch arch src)))

(define-syntax define-asm-fragment
  (syntax-rules ()
    [(_ name arch src)
     (define name (make-asm-fragment 'name 'arch src))]))

;; The runtime accessor is named <fragment-name>-tmpl.
(define (accessor-name sym)
  (symbol-append sym '-tmpl))

;; dump-asm-fragment :: <asm-fragment> [Port] -> ()
;;
;; Writes to PORT a comment block with the assembly listing followed by a
;; (define <name>-tmpl ...) form.  The defined function takes no arguments;
;; on first call it deserializes the embedded template data and caches the
;; result.  Subsequent calls return the cached <obj-template> directly.
(define (dump-asm-fragment frag :optional (port (current-output-port)))
  (format port ";; ~a\n" (~ frag'name))
  (display "#|\n" port)
  (with-output-to-port port
    (cut listing-for-arch (~ frag'arch) (~ frag'src)))
  (display "|#\n" port)
  (pprint
   `(define ,(accessor-name (~ frag'name))
      (let* ([data ',(serialize-obj-template (~ frag'template))]
             [p (delay ((module-binding-ref 'lang.asm.linker
                                            'deserialize-obj-template)
                        data))])
        (^[] (force p))))
   :port port
   :controls (make-write-controls :pretty #t :width 75
                                  :base 16 :radix #t))
  (newline port))
