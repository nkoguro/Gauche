;;;
;;; Object code template
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

;; Object code template is a result of in-process assembler. It contains
;; binary code bytearray, with information to 'patch' parts of the code
;; in the 'link' stage.

(define-module lang.asm.object
  (use gauche.uvector)
  (use binary.io)
  (use util.match)
  (export <obj-template> make-obj-template obj-template?
          link-template))
(select-module lang.asm.object)

;; Template class.
;;   bytes   - u8vector of assembled machine code (zeros at placeholder holes)
;;   labels  - alist of (symbol . byte-offset)
;;   patches - list of (keyword byte-offset byte-width) describing holes
;;     NB: The same keyword can appear multiple times in patches list.
(define-class <obj-template> ()
  ((bytes   :init-keyword :bytes :type <u8vector>)
   (labels  :init-keyword :labels)
   (patches :init-keyword :patches)))

(define (make-obj-template bytes labels patches)
  (make <obj-template> :bytes bytes :labels labels :patches patches))

(define (obj-template? x) (is-a? x <obj-template>))

;; TRANSIENT: cond-expand guard allows gen-native.scm to load this module
;; with BUILD_GOSH 0.9.15, which lacks native-ptr-fill!.  Remove after 0.9.16.
(define %native-ptr-fill!
  (cond-expand
    [gauche-0.9.15 (^ _ (error "native-ptr-fill! requires Gauche 0.9.16+"))]
    [else (module-binding-ref 'gauche.typeutil 'native-ptr-fill!)]))

;; fill-native-value! :: u8vector, int, int, <native-type>|<top>, val -> ()
;;   Fill BYTES from OFFSET spanning SIZE bytes with the binary representation
;;   of VALUE according to TYPE (always little-endian for numeric types).
(define (fill-native-value! bytes offset size type value)
  (define (bad)
    (error "Unsupported type to use in link-template:" type))
  (cond
   [(is-a? type <native-type>)
    (unless (<= (~ type 'size) size)
      (errorf "native type ~s doesn't fit in the patch size ~a" type size))
    (cond
     [(eq? (~ type'super) <integer>)
      (if (~ type'unsigned?)
        (case (~ type'size)
          [(1) (put-u8!    bytes offset value)]
          [(2) (put-u16le! bytes offset value)]
          [(4) (put-u32le! bytes offset value)]
          [(8) (put-u64le! bytes offset value)]
          [else (bad)])
        (case (~ type'size)
          [(1) (put-s8!    bytes offset value)]
          [(2) (put-s16le! bytes offset value)]
          [(4) (put-s32le! bytes offset value)]
          [(8) (put-s64le! bytes offset value)]
          [else (bad)]))]
     [(eq? type <float>)   (put-f32le! bytes offset value)]
     [(eq? type <double>)  (put-f64le! bytes offset value)]
     [(or (of-type? type <c-pointer>)
          (of-type? type <c-array>)
          (of-type? type <c-function>)
          (eq? type <c-string>))
      (%native-ptr-fill! bytes offset size type value)]
     [else (bad)])]
   [(eq? type <top>)
    (%native-ptr-fill! bytes offset size type value)]
   [else (bad)]))

;; Patch handler table: maps symbol -> (^ [bytes offset entry])
;;   bytes  - u8vector being patched (mutable copy)
;;   offset - byte offset of the patch location
;;   entry  - result of (assoc-ref params kw), or #f if not supplied
(define *patch-handlers* (make-hash-table 'eq?))

(define (register-patch-handler! key handler)
  (hash-table-set! *patch-handlers* key handler))

;; link-template :: <obj-template>, [(keyword native-type value [extra-offset])]
;;                  -> u8vector, AList
;;   Applies patches from PARAMS and returns the finalized byte vector and
;;   label alist.  The template is never mutated; a fresh u8vector is returned.
;;   Each param entry is either:
;;     (keyword native-type value)              -- fill at the patch's own offset
;;     (keyword native-type value extra-offset) -- fill at patch-offset + extra-offset
;;     (keyword c-array-type values)            -- fill array starting at patch's offset
;;     (keyword c-array-type values extra-offset) -- fill array starting at patch-offset + extra-offset
;;   The offset form lets callers fill locations derived from a named anchor.
(define (link-template tmpl params)
  (let* ([bytes (u8vector-copy (~ tmpl'bytes))]
         [len   (uvector-size bytes)])

    ;; Bounds-check ACTUAL then fill VALUE of NTYPE at that position.
    (define (checked-fill! kw actual ntype val)
      (when (>= actual len)
        (errorf "link-template: ~s adjusted offset ~a exceeds template size ~a"
                kw actual len))
      (fill-native-value! bytes actual (- len actual) ntype val))

    ;; Fill array VALS into consecutive element-sized slots starting at START.
    (define (fill-array! kw start atype vals)
      (unless (list? vals)
        (error "c-array parameter value must be a list:" vals))
      (let* ([etype (~ atype 'element-type)]
             [esize (~ etype 'size)])
        (let loop ([vs vals] [off start])
          (unless (null? vs)
            (checked-fill! kw off etype (car vs))
            (loop (cdr vs) (+ off esize))))))

    (dolist [patch (~ tmpl'patches)]
      (match patch
        ;; Typed patch: encode value according to native-type.
        [(kw base (? integer? width))
         (dolist [entry (filter (^e (eq? (car e) kw)) params)]
           (match entry
             [(_ (? (^x (of-type? x <c-array>)) atype) vals)
              (fill-array! kw base atype vals)]
             [(_ (? (^x (of-type? x <c-array>)) atype) vals (? integer? xoff))
              (fill-array! kw (+ base xoff) atype vals)]
             [(_ ntype val (? integer? xoff))
              (checked-fill! kw (+ base xoff) ntype val)]
             [(_ ntype val)
              (fill-native-value! bytes base width ntype val)]))]
        ;; Special patch: dispatch to registered handler.
        [(kw offset (? symbol? handler-key))
         (let1 handler (hash-table-get *patch-handlers* handler-key #f)
           (if handler
             (handler bytes offset (assoc-ref params kw))
             (error "unknown patch handler:" handler-key)))]))
    (values bytes (~ tmpl'labels))))

;;;
;;; Architecture-specific patch handlers
;;;

;; They are placed here instead of architecture-specific assembler
;; module.  This breaks modularity, but allows link-template used
;; without loading the assembler.

;; x86_64
;; Handler translates a symbol (movsd/#f -> #xf2, movss -> #xf3) into a
;; prefix byte at the given offset.
(register-patch-handler!
 'x86_64-movs_
 (^[bytes offset entry]
   (let* ([v   (if entry (cadr entry) 'movsd)]
          [pfx (case v
                 [(movsd) #xf2]
                 [(movss) #xf3]
                 [else (error "movs_ value must be movsd or movss:" v)])])
     (u8vector-set! bytes offset pfx))))
