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
  (export <obj-fragment> make-obj-fragment obj-fragment?
          <obj-template> make-obj-template obj-template?
          link-template link-templates))
(select-module lang.asm.object)

;; Fragment class.  Holds one contiguous section of assembled machine code.
;;   bytes   - u8vector of assembled bytes (zeros at placeholder holes)
;;   labels  - alist of (symbol . byte-offset), offsets relative to this fragment
;;   patches - list of patch descriptors, offsets relative to this fragment
;;   section - symbol naming the section (e.g. 'text, 'data)
(define-class <obj-fragment> ()
  ((bytes   :init-keyword :bytes   :type <u8vector>)
   (labels  :init-keyword :labels)
   (patches :init-keyword :patches)
   (section :init-keyword :section)))

(define (make-obj-fragment bytes labels patches section)
  (make <obj-fragment>
    :bytes bytes :labels labels :patches patches :section section))

(define (obj-fragment? x) (is-a? x <obj-fragment>))

;; Template class.  Holds one or more fragments plus shared endianness.
;;   fragments - list of <obj-fragment>
;;   endian    - one of the endianness symbols; can differ from (native-endian)
;;               when cross-assembling.
(define-class <obj-template> ()
  ((fragments :init-keyword :fragments)
   (endian    :init-keyword :endian)))

(define (make-obj-template fragments endian)
  (make <obj-template> :fragments fragments :endian endian))

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
(define (fill-native-value! bytes offset size type value endian)
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
          [(1) (put-u8!  bytes offset value)]
          [(2) (put-u16! bytes offset value endian)]
          [(4) (put-u32! bytes offset value endian)]
          [(8) (put-u64! bytes offset value endian)]
          [else (bad)])
        (case (~ type'size)
          [(1) (put-s8!  bytes offset value)]
          [(2) (put-s16! bytes offset value endian)]
          [(4) (put-s32! bytes offset value endian)]
          [(8) (put-s64! bytes offset value endian)]
          [else (bad)]))]
     [(eq? type <float>)   (put-f32! bytes offset value endian)]
     [(eq? type <double>)  (put-f64! bytes offset value endian)]
     ;; NB: We always fill pointer with native endian, for the
     ;; raw address won't make sense for cross assembling.
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
;;                  [#:postamble <int>]
;;                  -> u8vector, AList
;;   Applies patches from PARAMS and returns the finalized byte vector and
;;   label alist.  The template is never mutated; a fresh u8vector is returned.
;;   If POSTAMBLE > 0, the returned vector is extended by that many zero bytes
;;   beyond the template's own bytes (useful for per-call data regions).
;;   Each param entry is either:
;;     (keyword native-type value)              -- fill at the patch's own offset
;;     (keyword native-type value extra-offset) -- fill at patch-offset + extra-offset
;;     (keyword c-array-type values)            -- fill array starting at patch's offset
;;     (keyword c-array-type values extra-offset) -- fill array starting at patch-offset + extra-offset
;;   The offset form lets callers fill locations derived from a named anchor.
;; apply-patches! :: u8vector, symbol, patches, params -> ()
;;   Core patch application loop shared by link-template and link-templates.
(define (apply-patches! bytes endian patches params labels)
  (let1 len (uvector-size bytes)
    (define (checked-fill! kw actual ntype val)
      (when (>= actual len)
        (errorf "link-templates: ~s adjusted offset ~a exceeds template size ~a"
                kw actual len))
      (fill-native-value! bytes actual (- len actual) ntype val endian))

    (define (fill-array! kw start atype vals)
      (unless (list? vals)
        (error "c-array parameter value must be a list:" vals))
      (let* ([etype (~ atype 'element-type)]
             [esize (~ etype 'size)])
        (let loop ([vs vals] [off start])
          (unless (null? vs)
            (checked-fill! kw off etype (car vs))
            (loop (cdr vs) (+ off esize))))))

    (dolist [patch patches]
      (match patch
        [(kw base 'label-rel (? integer? end-off))
         ;; cross-section relative jump/reference: resolve target label
         (let1 target-addr (assq-ref labels kw #f)
           (unless target-addr
             (errorf "link-templates: label-rel: undefined label ~s" kw))
           (let1 disp (- target-addr end-off)
             (unless (ineq (- (expt 2 31)) <= disp < (expt 2 31))
               (errorf "link-templates: label-rel: displacement out of range for ~s" kw))
             (put-s32! bytes base disp endian)))]
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
              (fill-native-value! bytes base width ntype val endian)]))]
        [(kw offset (? symbol? handler-key))
         (let1 handler (hash-table-get *patch-handlers* handler-key #f)
           (if handler
             (handler bytes offset (assoc-ref params kw))
             (error "unknown patch handler:" handler-key)))]))))

;; link-templates :: (listof <obj-template>), params :key postamble -> u8vector, alist
;;   Merges fragments from all templates, grouped by section in first-seen order
;;   (A+C+B+D), rebases all label/patch offsets, applies params, and returns
;;   the finalized byte vector and merged label alist.
(define (link-templates tmpls params :key (postamble 0))
  (unless (pair? tmpls)
    (error "link-templates: template list must not be empty"))
  (let* ([endian (~ (car tmpls) 'endian)]
         [_      (for-each (^t (unless (eq? (~ t 'endian) endian)
                                 (error "link-templates: templates have mismatched endianness")))
                           (cdr tmpls))]
         ;; All fragments in input order.
         [frags    (append-map (^t (~ t 'fragments)) tmpls)]
         ;; Section order by first appearance.
         [sections (delete-duplicates (map (^f (~ f 'section)) frags) eq?)])
    ;; Walk sections in order.  For each section, walk all fragments (in order)
    ;; and pick those matching the current section, accumulating bytes, labels,
    ;; and patches with rebased offsets.
    (let loop ([secs sections] [offset 0]
               [rev-blists '()] [labels '()] [patches '()])
      (if (null? secs)
        ;; All sections processed: build the final vector and apply params.
        (let* ([byte-list (append-map identity (reverse rev-blists))]
               [total     (+ (length byte-list) postamble)]
               [bytes     (list->u8vector
                           (append byte-list (make-list postamble 0)))])
          (apply-patches! bytes endian patches params labels)
          (values bytes labels))
        (let1 sec (car secs)
          (let floop ([fs frags] [off offset]
                      [sec-blist '()] [sec-labels '()] [sec-patches '()])
            (if (null? fs)
              ;; Done with this section's fragments; advance to next section.
              (loop (cdr secs) off
                    (cons (reverse sec-blist) rev-blists)
                    (append labels sec-labels)
                    (append patches sec-patches))
              (let1 frag (car fs)
                (if (eq? (~ frag 'section) sec)
                  (let* ([fb   (~ frag 'bytes)]
                         [flen (uvector-size fb)])
                    (floop (cdr fs)
                           (+ off flen)
                           (append (reverse (u8vector->list fb)) sec-blist)
                           (append sec-labels
                                   (map (^p (cons (car p) (+ off (cdr p))))
                                        (~ frag 'labels)))
                           (append sec-patches
                                   (map (^p (match p
                                              [(kw disp 'label-rel end-off)
                                               (list kw (+ off disp) 'label-rel (+ off end-off))]
                                              [(kw disp . rest)
                                               (cons* kw (+ off disp) rest)]))
                                        (~ frag 'patches)))))
                  (floop (cdr fs) off
                         sec-blist sec-labels sec-patches))))))))))

;; link-template :: <obj-template>, params :key postamble -> u8vector, alist
;;   Single-template convenience wrapper around link-templates.
(define (link-template tmpl params :key (postamble 0))
  (link-templates (list tmpl) params :postamble postamble))

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
