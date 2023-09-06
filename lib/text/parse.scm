;;;
;;; parse.scm - utilities to parse input
;;;
;;;   Copyright (c) 2000-2023  Shiro Kawai  <shiro@acm.org>
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

;; This module implements the input parsing utilities described in Oleg's site
;;  http://pobox.com/~oleg/ftp
;; (follow the link of "Scheme Code -> Input parsing")
;;
;; The functions are API compatible with Oleg's library.  The performance of
;; this module is critical for the programs that parses large amount of
;; text, so the code here is tuned specifially to the Gauche compiler
;; to generate fast code.  (NB: We use some undocumented, experimenal
;; features.  Do not copy these techniques casually; we'll change the feature
;; at any time.)

(define-module text.parse
  (use srfi.13)
  (use scheme.charset)
  (use util.match)
  (export find-string-from-port?
          assert-curr-char
          skip-until
          skip-while
          peek-next-char
          next-token
          next-token-of
          read-string)
  )
(select-module text.parse)

(define ppp port-position-prefix)       ;for conciseness

(define (find-string-from-port? str in-port :optional (max-no-chars #f))

  (if (string-null? str)
    0                               ;special case
    (let ((restart (make-kmp-restart-vector str))
          (pattern (list->vector (string->list str)))
          (patlen  (string-length str)))

      (define (scan patpos count char)
        (cond ((eof-object? char) #f)
              ((char=? char (vector-ref pattern patpos))
               (if (= patpos (- patlen 1))
                 count
                 (scan (+ patpos 1) (+ count 1) (read-char in-port))))
              ((and max-no-chars (>= count max-no-chars)) #f)
              ((= patpos 0)
               (scan 0 (+ count 1) (read-char in-port)))
              (else
               (scan (vector-ref restart patpos) count char))
              ))

      (scan 0 1 (read-char in-port))
      )))

;; Given CHAR-LIST, returns a predicate that takes CHAR-LIST and a character,
;; and see if a character is included in the CHAR-LIST.
;; Oleg's original utility only allows characters and symbol *eof* in
;; CHAR-LIST.  We allow a single character set, or a list of mixture of
;; characters, character sets and symbol *eof*.
;;
;; This function, and the resulting predicate, can be called frequently
;; (it's O(n) where n is the size of the input to parse).  So we avoid
;; allocation, including closure creation.  Inlining char-list-predicate
;; also allows compile-time evaluation in majority of cases when char-list
;; is constant.
(define-inline (char-list-predicate char-list)
  (cond
   [(char-set? char-list) char-list-contains?/char-set]
   [(not (list? char-list))
    (error "CHAR-LIST must be a char-set or a list of characters, \
            char-sets and/or symbol '*eof*" char-list)]
   [(and (pair? char-list)              ; this pattern is generated by the
         (char-set? (car char-list))    ; compiler macros.
         (pair? (cdr char-list))
         (null? (cddr char-list))
         (eq? '*eof* (cadr char-list)))
    char-list-contains?/char-set/eof]
   [(memq '*eof* char-list)
    (if (every character-or-eof? char-list)
      char-list-contains?/chars/eof
      char-list-contains?/eof)]
   [(every char? char-list) char-list-contains?/chars]
   [else char-list-contains?]))

(define character-or-eof? (any-pred eof-object? char?))

(define (char-list-contains?/char-set char-list char)
  (and (char? char) (char-set-contains? char-list char)))
(define (char-list-contains?/char-set/eof char-list char) ; (#[...] *eof*)
  (or (eof-object? char)
      (and (char? char) (char-set-contains? (car char-list) char))))
(define (char-list-contains?/empty char-list char) #f)
(define (char-list-contains?/chars char-list char) (memv char char-list))
(define (char-list-contains?/chars/eof char-list char)
  (or (eof-object? char) (memv char char-list)))
(define (char-list-contains?/eof char-list char)
  (or (eof-object? char) (char-list-contains? char-list char)))
(define (char-list-contains? char-list char) ;generic version
  (let loop ((cs char-list))
    (if (null? cs)
      #f
      (or (eqv? (car cs) char)
          (and (char-set? (car cs))
               (char-list-contains?/char-set (car cs) char))
          (loop (cdr cs))))))

;; Common routine for the compiler macros.
(eval-when (:compile-toplevel :load-toplevel)
  (define (prefold-char-list char-list)
    (and (list? char-list)
         (cond [(every char? char-list) (apply char-set char-list)]
               [(every (any-pred char? (cut eq? <> '*eof*)) char-list)
                (list (apply char-set (delete '*eof* char-list)) '*eof*)]
               [else #f])))

  (define (prefold-macro-1 form r c)
    (match form
      [(op ('quote cs) . args)
       (or (and-let* ([cs. (prefold-char-list cs)])
             `(,(r (symbol-append '% op)) ',cs. ,@args))
           `(,(r (symbol-append '% op)) ',cs ,@args))]
      [(op . x) `(,(r (symbol-append '% op)) ,@x)]))

  (define (prefold-macro-2 form r c)
    (match form
      [(op ('quote cs1) ('quote cs2) . args)
       (or (and-let* ([cs1. (prefold-char-list cs1)]
                      [cs2. (prefold-char-list cs2)])
             `(,(r (symbol-append '% op)) ',cs1. ',cs2. ,@args))
           `(,(r (symbol-append '% op)) ',cs1 ',cs2 ,@args))]
      [(op . x) `(,(r (symbol-append '% op)) ,@x)]))
  )

;; ASSERT-CURR-CHAR <char-list> <string> :optional <port>
(define-inline (%assert-curr-char char-list string
                                  :optional (port (current-input-port)))
  (define pred (char-list-predicate char-list))
  (rlet1 c (read-char port)
    (unless (pred char-list c)
      (errorf "~awrong character ~s ~a. ~s expected."
              (ppp port) c string char-list))))

(define-hybrid-syntax assert-curr-char
  %assert-curr-char (er-macro-transformer prefold-macro-1))

;; SKIP-UNTIL <char-list/number/pred> :optional <port>
(define-inline (%skip-until char-list/number/pred
                            :optional (port (current-input-port)))
  (cond
   [(number? char-list/number/pred)
    (skip-until/number char-list/number/pred port)]
   [(procedure? char-list/number/pred)
    (skip-until/pred char-list/number/pred port)]
   [else
    (skip-until/char-list (char-list-predicate char-list/number/pred)
                          char-list/number/pred port)]))

(define-hybrid-syntax skip-until
  %skip-until (er-macro-transformer prefold-macro-1))

(define (skip-until/number num port)
  (and (<= 1 num)
       (let loop ([i 1] [c (read-char port)])
         (cond [(eof-object? c) (errorf "~aunexpected EOF" (ppp port))]
               [(>= i num) #f]
               [else (loop (+ i 1) (read-char port))]))))

(define-inline (skip-until/common pred port)
  (let loop ([c (read-char port)])
    (cond [(pred c) c]
          [(eof-object? c) (errorf "~aunexpected EOF" (ppp port))]
          [else (loop (read-char port))])))
(define skip-until/pred skip-until/common);trick to prevent excessive inlining
(define (skip-until/char-list pred char-list port)
  (skip-until/common (cut pred char-list <>) port))


;; SKIP-WHILE <char-list/pred> :optional <port>
(define-inline (%skip-while char-list/pred
                            :optional (port (current-input-port)))
  (cond
   [(procedure? char-list/pred) (skip-while/pred char-list/pred port)]
   [else (skip-while/char-list (char-list-predicate char-list/pred)
                               char-list/pred port)]))

(define-hybrid-syntax skip-while
  %skip-while (er-macro-transformer prefold-macro-1))

(define-inline (skip-while/common pred port)
  (let loop ([c (peek-char port)])
    (cond [(pred c) (read-char port) (loop (peek-char port))]
          [else c])))
(define skip-while/pred skip-while/common)
(define (skip-while/char-list pred char-list port)
  (skip-while/common (cut pred char-list <>) port))

;; PEEK-NEXT-CHAR :optional <port>
(define-inline (peek-next-char :optional (port (current-input-port)))
  (read-char port)
  (peek-char port))

;; NEXT-TOKEN <prefix-char-list/pred> <break-char-list/pred>
;;            :optional <comment> <port>
(define-inline (%next-token prefix-char-list/pred break-char-list/pred
                            :optional (comment "unexpected EOF")
                                      (port (current-input-port)))
  (let1 c (skip-while prefix-char-list/pred port)
    (if (procedure? break-char-list/pred)
      (next-token/pred break-char-list/pred c port comment)
      (next-token/char-list (char-list-predicate break-char-list/pred)
                            break-char-list/pred c port comment))))

(define-hybrid-syntax next-token
  %next-token (er-macro-transformer prefold-macro-2))

(define-inline (next-token/common break-pred char port errmsg)
  (define o (open-output-string))
  (let loop ([c char])
    (cond [(break-pred c) (get-output-string o)]
          [(eof-object? c) (errorf "~a~a" (ppp port) errmsg)]
          [else (write-char c o) (read-char port) (loop (peek-char port))])))
(define next-token/pred next-token/common)
(define (next-token/char-list pred char-list char port errmsg)
  (next-token/common (cut pred char-list <>) char port errmsg))

;; NEXT-TOKEN-OF <char-list/pred> :optional <port>
(define-inline (%next-token-of char-list/pred
                              :optional (port (current-input-port)))
  (if (procedure? char-list/pred)
    (next-token-of/pred char-list/pred port)
    (next-token-of/char-list (char-list-predicate char-list/pred)
                             char-list/pred port)))

(define-hybrid-syntax next-token-of
  %next-token-of (er-macro-transformer prefold-macro-1))

(define-inline (next-token-of/common pred port)
  (define o (open-output-string))
  (let loop ([c (peek-char port)])
    (cond [(or (eof-object? c) (not (pred c))) (get-output-string o)]
          [else (write-char c o) (read-char port) (loop (peek-char port))])))
(define next-token-of/pred next-token-of/common)
(define (next-token-of/char-list pred char-list port)
  (next-token-of/common (cut pred char-list <>) port))


;; read-line is built-in.

;; this is slightly different from built-in read-string
(define (read-string n :optional (port (current-input-port)))
  (let1 s ((with-module gauche read-string) n port)
    (if (eof-object? s)
      ""
      s)))
