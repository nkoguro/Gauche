;; test for advanced read/write features
;;
;; These test is splitted from io.scm, since they depend on other modules
;; has to be done after their tests.

(use gauche.test)
(use scheme.list)
(use file.util)
(use util.isomorph)

(test-start "advanced read/write features")

;;===============================================================
;; Hash-bang handling (#!)
;;

(test-section "hash-bang")

(test* "script hash-bang" 3
       (read-from-string "#!/usr/bin/gosh -i\n3"))
(test* "script hash-bang" 5
       (read-from-string "#! /usr/bin/gosh -i\n5"))
(test* "script hash-bang" (eof-object)
       (read-from-string "#! /usr/bin/gosh -i"))
(test* "script hash-bang error1" (test-error)
       (read-from-string "#!"))

(test* "#!fold-case" '(hello world)
       (read-from-string "#!fold-case (Hello World)"))
(test* "#!fold-case" '(Hello world)
       (read-from-string "(Hello #!fold-case World)"))
(test* "#!no-fold-case" '(hello World)
       (read-from-string "(#!fold-case Hello #!no-fold-case World)"))

(define-reader-directive 'usr/bin/gosh (lambda _ (values)))

(test* "customized hash-bang" -i
       (read-from-string "#!usr/bin/gosh -i\n8"))

(define-reader-directive 'true!  (lambda _ #t))
(define-reader-directive 'false! (lambda _ #f))

(test* "customized hash-bang" '(#t #f)
       (read-from-string "#!/usr/bin/gosh -i\n(#!true! #!false!)"))

(test* "customized hash-bang error1" (test-error)
       (read-from-string "#!/usr/bin/gosh -i\n#! true!"))
(test* "customized hash-bang error2" (test-error)
       (read-from-string "#!/usr/bin/gosh -i\n#!#:true!"))

;;===============================================================
;; SRFI-10 Reader constructor (#,)
;;

(test-section "SRFI-10 reader constructor")

(test "read ctor 1a" '(1 2 #f "4 5")
      (lambda ()
        (define-reader-ctor 'list list)
        (with-input-from-string "#,(list 1 2 #f \"4 5\")" read)))
(test "read ctor 1b" 3
      (lambda ()
        (define-reader-ctor '+ +)
        (with-input-from-string "#,(+ 1 2)" read)))
(define-reader-ctor 'my-vector
  (lambda x (apply vector (cons 'my-vector x))))
(test* "read ctor 2a" '#(my-vector (my-vector 1 2))
       (with-input-from-string "#,(my-vector (my-vector 1 2))" read))
(test* "read ctor 2b" '#(my-vector #(my-vector 1 2))
       (with-input-from-string "#,(my-vector #,(my-vector 1 2))" read))

;;===============================================================
;; Shared structures (#n= and #n#)
;;

;;---------------------------------------------------------------
(test-section "write/ss basic")

(test* "pair" "(#0=(a b) #0#)"
       (let1 x '(a b)
         (write-to-string (list x x) write/ss)))
(test* "pair" "(#0=(a b) . #0#)"
       (let1 x (list 'a 'b)
         (write-to-string (cons x x) write/ss)))
(test* "pair" "(#0=(a b) #1=(a b) #0# . #1#)"
       (let ((x (list 'a 'b))
             (y (list 'a 'b)))
         (write-to-string (list* x y x y) write/ss)))
(test* "pair (circular)" "#0=(a . #0#)"
       (let1 x (list 'a 'b)
         (set-cdr! x x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(#0# b)"
       (let1 x (list 'a 'b)
         (set-car! x x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(#0# . #0#)"
       (let1 x (list 'a 'b)
         (set-car! x x)
         (set-cdr! x x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(a (b . #0#))"
       (let1 x (list 'a (list 'b 'c))
         (set-cdr! (cadr x) x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(a #1=(b . #0#) . #1#)"
       (let1 x (list 'a (list 'b 'c))
         (set-cdr! (cadr x) x)
         (set-cdr! (cdr x) (cadr x))
         (write-to-string x write/ss)))

(test* "vector" "(#0=#(a b) . #0#)"
       (let1 x (vector 'a 'b)
         (write-to-string (cons x x) write/ss)))
(test* "vector" "(#() . #())"
       (let1 x (vector)
         (write-to-string (cons x x) write/ss)))
(test* "vector" "#(#0=(a b) #0# #0#)"
       (let1 x '(a b)
         (write-to-string (vector x x x) write/ss)))
(test* "vector (circular)" "#0=#(#0#)"
       (let1 x (vector 0)
         (vector-set! x 0 x)
         (write-to-string x write/ss)))

(test* "string" "(#0=\"ab\" . #0#)"
       (let1 x "ab"
         (write-to-string (cons x x) write/ss)))
(test* "string" "(\"\" . \"\")"
       (let1 x ""
         (write-to-string (cons x x) write/ss)))

(test* "mixed" "#0=(a #1=#(#2=\"xyz\" #0# #1# #2#) . #0#)"
       (let* ([a (list 'a)]
              [b "xyz"]
              [c (vector b a 'x b)])
         (set-cdr! a (cons c a))
         (vector-set! c 2 c)
         (write-to-string a write/ss)))
(test* "deeply nested" "#0=(((#1=(#0# #0# z) #1# y) . #2=(#1# #0# . x)) #2#)"
       (let* ([a (list 'a 'a)]
              [b (list a a 'z)]
              [c (list b b 'y)]
              [d (list* c b a 'x)])
         (set-car! a d)
         (set-car! (cdr a) (cdr d))
         (write-to-string a write/ss)))

(test* "more than 10 substructures"
       "(#0=(a) #1=(b) #2=(c) #3=(d) #4=(e) #5=(f) #6=(g) #7=(h) #8=(i) #9=(j) #10=(k) #10# #9# #8# #7# #6# #5# #4# #3# #2# #1# #0#)"
       (let ((a '(a)) (b '(b)) (c '(c)) (d '(d)) (e '(e))
             (f '(f)) (g '(g)) (h '(h)) (i '(i)) (j '(j)) (k '(k)))
         (write-to-string
          (list a b c d e f g h i j k
                k j i h g f e d c b a)
          write/ss)))

(test* "circular list involving abbrev syntax" "#0=((quote . #0#))"
       (write-to-string (cdr #0='#0#) write/ss))


(let* ([ht1 (make-hash-table 'eq?)]
       [ht2 (make-hash-table 'eq?)]
       [circ (list ht1 ht2)])
  (hash-table-put! ht1 'circ circ)
  (hash-table-put! ht1 'ht1 ht1)
  (hash-table-put! ht2 'circ (cdr circ))
  (hash-table-put! ht2 'ht1 ht1)
  (test* "circular structure involving hashtable"
         "#0=(#1=#<hash-table eq[2] @> . #2=(#<hash-table eq[2] @>))"
         (regexp-replace-all #/@(0x)?[0-9a-f]+/
                             (write-to-string circ write/ss)
                             "@")))

(define-class <foo> ()
  ((a :init-keyword :a)
   (b :init-keyword :b)))
(define-method write-object ((self <foo>) port)
  (format port "#,(foo ~s ~s)" (ref self 'a) (ref self 'b)))

(test* "user defined" "#,(foo #0=(a b) #0#)"
       (let* ((x '(a b))
              (foo (make <foo> :a x :b x)))
         (write-to-string foo write/ss)))
(test* "user defined" "#0=#,(foo #0# #0#)"
       (let ((foo (make <foo> :a #f :b #f)))
         (set! (ref foo 'a) foo)
         (set! (ref foo 'b) foo)
         (write-to-string foo write/ss)))
(test* "user defined" "#0=#,(foo foo #,(foo bar #0#))"
       (let* ((foo (make <foo> :a 'foo :b #f))
              (bar (make <foo> :a 'bar :b foo)))
         (set! (ref foo 'b) bar)
         (write-to-string foo write/ss)))
(test* "user defined" "(#0=#,(foo foo #1=#,(foo bar #0#)) #1#)"
       (let* ((foo (make <foo> :a 'foo :b #f))
              (bar (make <foo> :a 'bar :b foo)))
         (set! (ref foo 'b) bar)
         (write-to-string (list foo bar) write/ss)))
(test* "user defined" "#0=(#1=#,(foo #2=#,(foo bar #1#) #0#) #2#)"
       (let* ((foo (make <foo> :a 'foo :b #f))
              (bar (make <foo> :a 'bar :b foo))
              (baz (list foo bar)))
         (set! (ref foo 'a) bar)
         (set! (ref foo 'b) baz)
         (write-to-string baz write/ss)))

;; write/ss with user-defined write-object method.
;; test by UEYAMA Rui
(define-class <bar> ()
  ((a :init-keyword :a)
   (b :init-keyword :b)))
(define-method write-object ((self <bar>) port)
  (display "#,(bar " port)
  (write/ss (ref self 'a) port)
  (display " " port)
  (write/ss (ref self 'b) port)
  (display ")" port))
(test* "user defined" "#,(bar #0=(a b) #0#)"
       (let* ((x '(a b))
              (bar (make <bar> :a x :b x)))
         (write-to-string bar write/ss)))

(define-class <baz> ()
  ((a :init-keyword :a)
   (b :init-keyword :b)))
(define-method write-object ((self <baz>) port)
  (display "#," port)
  (write `(baz :a ,(ref self 'a) :b ,(ref self 'b)) port))
(test* "user defined mixed"
       "#,(baz :a (#0=#,(foo foo foob) #1=#,(foo fee feeb)) :b (#,(bar #0# #1#) #,(bar #1# #0#)))"
       (let* ((f0 (make <foo> :a 'foo :b 'foob))
              (f1 (make <foo> :a 'fee :b 'feeb))
              (b1 (make <bar> :a f0 :b f1))
              (b2 (make <bar> :a f1 :b f0))
              (c  (make <baz> :a (list f0 f1) :b (list b1 b2))))
         (write-to-string c write/ss)))

;; This test doesn't involve shared structure.  It is to test
;; we can handle deep list without busting C stack.
(test* "deep list doesn't bust C stack" 2000002
       (let loop ([cnt 0] [ls '()])
         (if (< cnt 1000000)
           (loop (+ cnt 1) (list ls))
           (string-length (write-to-string ls)))))

;;---------------------------------------------------------------
(test-section "format/ss")

(test* "format/ss" "The answer is #0=(\"a\" . #0#)"
       (let ((a (list "a")))
         (set-cdr! a a)
         (format/ss "The answer is ~s" a)))

(test* "format/ss" "The answer is #0=(a . #0#)"
       (let ((a (list "a")))
         (set-cdr! a a)
         (format/ss "The answer is ~a" a)))

(test* "format/ss" "The answer is #0=(a . #0#) #0#"
       (let ((a (list 'a)))
         (set-cdr! a a)
         (format/ss "The answer is ~s ~s" a a)))

;;---------------------------------------------------------------
(test-section "obscure format features")

(test* "format plural" "0 books, 1 book, 2 books, 1.0 books"
       (format "~a book~p, ~a book~p, ~a book~p, ~a book~p"
               0 0 1 1 2 2 1.0 1.0))
(test* "format plural" "0 pens, 1 pen, 2 pens, 1.0 pens"
       (format "~a pen~:p, ~a pen~:p, ~a pen~:p, ~a pen~:p"
               0 1 2 1.0))
(test* "format plural" "0 ponies, 1 pony, 2 ponies, 1.0 ponies"
       (format "~a pon~:@p, ~a pon~:@p, ~a pon~:@p, ~a pon~:@p"
               0 1 2 1.0))

(test* "format roman numerals"
       '("I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X")
       (map (^i (format "~@R" i)) (iota 10 1)))
(test* "format roman numerals"
       '("i" "ii" "iii" "iv" "v" "vi" "vii" "viii" "ix" "x")
       (map (^i (format "~@r" i)) (iota 10 1)))
(test* "format roman numerals"
       '("I" "II" "III" "IIII" "V" "VI" "VII" "VIII" "VIIII" "X")
       (map (^i (format "~:@R" i)) (iota 10 1)))
(test* "format roman numerals"
       '("X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC" "C")
       (map (^i (format "~@R" i)) (iota 10 10 10)))
(test* "format roman numerals"
       '("C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM" "M")
       (map (^i (format "~@R" i)) (iota 10 100 100)))

(let1 data '((39 "XXXIX")
             (246 "CCXLVI")
             (789 "DCCLXXXIX")
             (2421 "MMCDXXI")
             (160 "CLX")
             (207 "CCVII")
             (1009 "MIX")
             (1066 "MLXVI")
             (1776 "MDCCLXXVI")
             (1918 "MCMXVIII")
             (2014 "MMXIV"))
  (dolist [d data] (test* "format roman numeral" (cadr d)
                          (format "~@R" (car d)))))

(test* "format conditionals (case)"
       '("It is a cat."
         "It is a Siamese cat."
         "It is a Manx cat."
         "It is a Persian cat."
         "It is a cat.")
       (map (cut format "It is a ~[Siamese ~;Manx ~;Persian ~]cat." <>)
            '(-1 0 1 2 3)))
(test* "format conditionals (case, default)"
       '("It is a Calico cat."
         "It is a Siamese cat."
         "It is a Manx cat."
         "It is a Persian cat."
         "It is a Calico cat.")
       (map (cut format "It is a ~[Siamese ~;Manx ~;Persian ~:;Calico ~]cat." <>)
            '(-1 0 1 2 3)))
(test* "format conditionals (case, error)"
       (test-error <error> #/Argument for ~\[ must/)
       (format "It is a ~[Siamese ~;Manx ~;Persian ~]cat." 'a))

(test* "format conditionals (alterative)"
       '("Green Lamp" "Blue \"Lamp\"")
       (map (cut format "~:[Green ~a~;Blue ~s~]" <> "Lamp") '(#f #t)))

(test* "format conditionals (maybe)"
       '("Don't worry." "It's OK. Don't worry.")
       (map (cut format "~@[It's ~a. ~]~a." <> "Don't worry") '(#f "OK")))

;; Ensure `format` does _not_ cache in this case.
(test* "formatter cache"
       '("a" "~s")
       (map (cut format <> "~s" 'a) '(#f "~0@*~a")))

;;---------------------------------------------------------------
(test-section "read/ss basic")

;; NB: in gauche, read/ss is just an alias of read.
(test* "scalar (harmless)" 0
       (read-from-string "#0=0"))
(test* "scalar (harmless)" 1
       (read-from-string "#1=1"))
(test* "scalar (harmless)" 2
       (read-from-string "#0=#1=2"))
(test* "scalar (harmless)" #f
       (read-from-string "#1=#10=#100=#f"))
(test* "scalar (harmless)" "aaa"
       (read-from-string "#1=#0=\"aaa\""))

(test* "bad syntax" (test-error)
       (read-from-string "#1"))
(test* "bad syntax" (test-error)
       (read-from-string "#3#"))
(test* "bad syntax" (test-error)
       (read-from-string "#99999999999999999999999999999999999=3"))
(test* "bad syntax" (test-error)
       (read-from-string "#99999999999999999999999999999999999#"))

(test* "pair 1" (circular-list 1 2)
       (read-from-string "#0=(1 2 . #0#)")
       isomorphic?)
(test* "pair 2" (let1 r (list #f) (set! (car r) r) r)
       (read-from-string "#0=(#0#)")
       isomorphic?)
(test* "pair 3" (let1 r '(a b) (list r r r))
       (read-from-string "(#0=#1=(a b) #0# #1#)")
       isomorphic?)

(test* "vector" (let* ((r (vector 'a 'b))
                       (s (vector 'c 'd))
                       (t (vector r s r s 'e)))
                  (vector-set! r 1 s)
                  (vector-set! s 1 r)
                  (vector-set! t 4 t)
                  t)
       (read-from-string "#0=#(#1=#(a #2=#(c #1#)) #2# #1# #2# #0#)")
       isomorphic?)

(test* "string" (let* ((r (string #\a #\a))
                       (s (string #\a #\a)))
                  (list r s r s))
       (read-from-string "(#0=\"aa\" #1=\"aa\" #0# #1#)")
       isomorphic?)

(test-section "whitespaces")

(test* "skipws" 'a
       (read-from-string
        "\u00a0\u1680\u2000\u200a\u2028\u2029\u202f\u205f\u3000a"))

;;===============================================================
;; Interference between SRFI-10 and shared structure
;;

(test-section "combine SRFI-10 and SRFI-38")

;; NB: this is an experimental feature.  Do not count on this API!
(define-reader-ctor 'foo
  (lambda x `(quote ,x))
  (lambda (obj)
    (pair-for-each (lambda (p)
                     (when (read-reference? (car p))
                       (set-car! p (read-reference-value (car p)))))
                   (cadr obj))))

(test* "user-defined" '#0='(a #0#)
       (read-from-string "#0=#,(foo a #0#)")
       isomorphic?)


;;===============================================================
;; Read lexical mode
;;

(test-section "read lexical mode")

(define (test-reader-lexical-mode mode input expect)
  (test* (format "reader lexical mode ~s: ~s" mode input) expect
         (let1 old-mode #f
           (dynamic-wind
             (^[] (set! old-mode (reader-lexical-mode mode)))
             (^[] (guard (e [else 'error])
                    (let* ([warn-port (open-output-string)]
                           [r (with-error-to-port warn-port
                                (cut read-from-string input))]
                           [w (rxmatch-case (get-output-string warn-port)
                                [#/^WARNING/ () 'warn]
                                [else #f])])
                      (if w (list r w) r))))
             (^[] (reader-lexical-mode old-mode))))))

;; data ::= ((input expect ...) ...)
(define (test-reader-lexical-modes data)
  (dolist [d data]
    (for-each (^[m e] (test-reader-lexical-mode m (car d) e))
              '(legacy permissive warn-legacy strict-r7)
              (cdr d))))

(test-reader-lexical-modes
 '(("\"a\\x30zz\"" "a0zz" "a0zz" ("a0zz" warn) error)))
(test-reader-lexical-modes
 '(("\"a\\x30;zz\"" "a0;zz" "a0zz" "a0zz" "a0zz")))
(test-reader-lexical-modes
 '(("\"a\\x0030zz\"" "a\030zz" "a\030zz" ("a\030zz" warn) error)))
(test-reader-lexical-modes
 '(("\"a\\x0030;zz\"" "a\030;zz" "a0zz" "a0zz" "a0zz")))

;; Load and reader-lexical-mode
(sys-unlink "test.o")
(sys-unlink "test1.o")

(with-output-to-file "test.o"
  (lambda ()
    (write '(reader-lexical-mode 'legacy))))

(test* "load restores read lexical mode" #t
       (let1 x (reader-lexical-mode)
         (load "./test.o")
         (eq? (reader-lexical-mode) x)))

(define (test-reader-lexical-mode-directive directive literal)
  (with-output-to-file "test.o"
    (lambda ()
      (display directive)
      (display "\n")
      (display "(with-output-to-file \"test1.o\" (lambda () (display ")
      (display literal)
      (display ")))")))
  (load "./test.o")
  (with-input-from-file "test1.o" (cut read-line)))

(test* "#!gauche-legacy directive" "0;z"
       (test-reader-lexical-mode-directive "#!gauche-legacy" "\"\\x30;z\""))
(test* "#!r7rs directive" "0z"
       (test-reader-lexical-mode-directive "#!r7rs" "\"\\x30;z\""))
(test* "#!r7rs directive" (test-error <read-error>)
       (test-reader-lexical-mode-directive "#!r7rs" "\"\\x30\""))
(test* "#!r7rs directive effect remains local to the port"
       '("0z" "0z")
       (let* ([a (read-from-string "#!r7rs \"\\x30;z\"")]
              [b (read-from-string "\"\\x30z\"")])
         (list a b)))

(sys-unlink "test.o")
(sys-unlink "test1.o")

;;===============================================================
;; Write parameters
;;

(use gauche.uvector)

(let* ([data (iota 5)]
       [data2 (make-list 5 data)])
  (define (write-to-string/ctx obj . args)
    ($ write-to-string obj
       (^x (write x (apply make-write-controls args)))))
  (define elli (with-module gauche.internal (string-ellipsis)))

  (test* "print-length"
         `("(0 1 2 3 4)"
           ("(0 1 2 3 4)"       "#(0 1 2 3 4)"       "#u8(0 1 2 3 4)")
           (,#"(0 1 2 3 ~elli)" ,#"#(0 1 2 3 ~elli)" ,#"#u8(0 1 2 3 ~elli)")
           (,#"(0 1 2 ~elli)"   ,#"#(0 1 2 ~elli)"   ,#"#u8(0 1 2 ~elli)")
           ("(0 1 2 3 4)"       "#(0 1 2 3 4)"       "#u8(0 1 2 3 4)")
           (,#"(0 1 ~elli)"     ,#"#(0 1 ~elli)"     ,#"#u8(0 1 ~elli)")
           (,#"(0 ~elli)"       ,#"#(0 ~elli)"       ,#"#u8(0 ~elli)")
           (,#"(~elli)"         ,#"#(~elli)"         ,#"#u8(~elli)"))
         (let ([z (map (^n (list (write-to-string/ctx data :length n)
                                 (write-to-string/ctx (list->vector data)
                                                      :length n)
                                 (write-to-string/ctx (list->u8vector data)
                                                      :length n)))
                       '(5 4 3 #f 2 1 0))])
           ;; make sure print-length doesn't affect global op
           (cons (write-to-string data)
                 z)))

  (test* "print-length for zero-length aggregate"
         '("()" "#()" "#u8()")
         (map (^x (write-to-string/ctx x :length 0))
              '(() #() #u8())))

  (test* "print-length (nested)"
         `((,#"(~elli)"
            ,#"#(~elli)")
           (,#"((0 ~elli) ~elli)"
            ,#"#(#(0 ~elli) ~elli)")
           (,#"((0 1 ~elli) (0 1 ~elli) ~elli)"
            ,#"#(#(0 1 ~elli) #(0 1 ~elli) ~elli)")
           (,#"((0 1 2 ~elli) (0 1 2 ~elli) (0 1 2 ~elli) ~elli)"
            ,#"#(#(0 1 2 ~elli) #(0 1 2 ~elli) #(0 1 2 ~elli) ~elli)"))
         (map (^n (list (write-to-string/ctx data2 :length n)
                        (write-to-string/ctx
                         (list->vector (map list->vector data2))
                         :length n)))
              (iota 4))))

;; example from CLHS
(let* ([data '(1 (2 (3 (4 (5 (6))))))])
  (test* "print-level"
         '("#"
           "(1 #)"
           "(1 (2 #))"
           "(1 (2 (3 #)))"
           "(1 (2 (3 (4 #))))"
           "(1 (2 (3 (4 (5 #)))))"
           "(1 (2 (3 (4 (5 (6))))))"
           "(1 (2 (3 (4 (5 (6))))))")
         (map (^n (write-to-string data (^x (write x (current-output-port)
                                                   (make-write-controls
                                                    :level n)))))
              (iota 8))))

(let* ([data '(a (b (c (d (e) (f) g) h) i) #(j (k #(l #(m) (n) o) p) q) r)])
  (test* "print-level"
         '("(a (b (c (d (e) (f) g) h) i) #(j (k #(l #(m) (n) o) p) q) r)"
           "(a (b (c (d (e) (f) g) h) i) #(j (k #(l #(m) (n) o) p) q) r)"
           "(a (b (c (d # # g) h) i) #(j (k #(l # # o) p) q) r)"
           "(a (b (c # h) i) #(j (k # p) q) r)"
           "(a (b # i) #(j # q) r)"
           "(a # # r)"
           "#")
         (map (^n (write-to-string data (^x (write x (make-write-controls
                                                      :level n)
                                                   (current-output-port)))))
              '(6 5 4 3 2 1 0))))

;; another example from CLHS
(let* ([level-length '((0 1) (1 1) (1 2) (1 3) (1 4)
                       (2 1) (2 2) (2 3) (3 2) (3 3) (3 4))]
       [data '(if (member x y) (+ (car x) 3) '(foo . #(a b c d "Baz")))])
  (define elli (with-module gauche.internal (string-ellipsis)))
  (test* "print-level & print-length"
         `(,#"0 1 -- #"
           ,#"1 1 -- (if ~elli)"
           ,#"1 2 -- (if # ~elli)"
           ,#"1 3 -- (if # # ~elli)"
           ,#"1 4 -- (if # # #)"
           ,#"2 1 -- (if ~elli)"
           ,#"2 2 -- (if (member x ~elli) ~elli)"
           ,#"2 3 -- (if (member x y) (+ # 3) ~elli)"
           ,#"3 2 -- (if (member x ~elli) ~elli)"
           ,#"3 3 -- (if (member x y) (+ (car x) 3) ~elli)"
           ,#"3 4 -- (if (member x y) (+ (car x) 3) '(foo . #(a b c d ~elli)))")
         (map (^z (let1 c (make-write-controls
                           :level (car z) :length (cadr z))
                    (format c "~d ~d -- ~s" (car z) (cadr z) data)))
              level-length)))

;; print-level and aggregate other than plain vector
(let ([data '(a #u8(1 2 3) (b #u16(1 2 3) #(c #u32(1 2 3))))])
  (test* "print-level with uvector"
         '("(a #u8(1 2 3) (b #u16(1 2 3) #(c #u32(1 2 3))))"
           "(a #u8(1 2 3) (b #u16(1 2 3) #(c #)))"
           "(a #u8(1 2 3) (b # #))"
           "(a # #)"
           "#")
         (map (^n (write-to-string data
                                   (^x (write x (current-output-port)
                                              (make-write-controls
                                               :level n)))))
              '(4 3 2 1 0))))

;; print-level and user-defined write method
(define-class <foo> ()
  ((content :init-keyword :content)))
(define-method write-object ((obj <foo>) port)
  (format port "#<foo ~s>" (~ obj'content)))
(define-class <bar> ()
  ((content :init-keyword :content)))
(define-method write-object ((obj <bar>) port)
  (display "#<bar " port)
  (write (~ obj'content) port)
  (display ">" port))

(let ([data (list '(1 (2 (3 4)))
                  (list 1
                        (make <bar> :content
                              (list 2 '(3 4))))
                  (list 1
                        (make <foo> :content
                              (list 2 '(3 4))))
                  (make <foo> :content (list '(2 (3 (4 5)))
                                             (make <bar> :content
                                                   '(2 (3 (4 5)))))))])
  (test* "print-level via write-object"
         '("((1 (2 (3 4))) (1 #<bar (2 (3 4))>) (1 #<foo (2 (3 4))>) #<foo ((2 (3 (4 5))) #<bar (2 (3 (4 5)))>)>)"
           "((1 (2 (3 4))) (1 #<bar (2 (3 4))>) (1 #<foo (2 (3 4))>) #<foo ((2 (3 #)) #<bar (2 (3 #))>)>)"
           "((1 (2 #)) (1 #<bar (2 #)>) (1 #<foo (2 #)>) #<foo ((2 #) #<bar (2 #)>)>)"
           "((1 #) (1 #<bar #>) (1 #<foo #>) #<foo (# #<bar #>)>)"
           "(# # # #<foo #>)"
           "#")
         (map (^n (write-to-string data
                                   (^x (write x (make-write-controls
                                                 :level n)))))
              '(5 4 3 2 1 0))))

(define-class <baz> ()
  ((content :init-keyword :content)))
(define *baz-log* #f)
(define-method write-object ((obj <baz>) port)
  (display "#<baz " port)
  ;; don't do this in real code; this is only for testing
  (let1 wc (make-write-controls :base 16)
    (write (~ obj'content) wc port)
    (unless ((with-module gauche.internal %port-walking?) port)
      (write (~ obj'content) wc *baz-log*)))
  (display ">" port))

(let ([data (list 10 20 (make <baz> :content '(30 40 50)))])
  (test* "recursive write, control overrides"
         '("(101 202 #<baz (1010 1111 1212)>)"
           "(1e 28 32)")
         (let ([p1 (open-output-string)]
               [p2 (open-output-string)])
           (set! *baz-log* p2)
           (write data p1 (make-write-controls :base 3))
           (list (get-output-string p1)
                 (get-output-string p2)))))

;;===============================================================
;; peek-byte - read-char
;;

(use gauche.charconv)

(let ()
  ;; This test checks peek-byte won't disturb subsequent read-char
  ;; even the input contains illegal byte sequence.
  ;; related: https://github.com/shirok/Gauche/pull/177
  (define (read1 p) (read-char p))
  (define (read2 p) (let1 c (read-char p) `(,c ,(read-char p))))
  (define (peek1 p) (peek-char p) (read1 p))
  (define (peek2 p) (peek-char p) (read2 p))
  (define (fetch s reader ces)
    (call-with-input-string (ces-convert s #f ces) reader))

  (define (t ces)
    (test* #"peek-char - read-char (~ces)"
           (fetch "\u3042" read1 ces) (fetch "\u3042" peek1 ces))
    (test* #"peek-char - read-char^2 (~ces) case 1"
           (fetch "\u3042" read2 ces) (fetch "\u3042" peek2 ces))
    (test* #"peek-char - read-char^2 (~ces) case 2"
           (fetch "\u3042\u3044" read2 ces)
           (fetch "\u3042\u3044" peek2 ces)))

  (for-each t '(utf8 sjis eucjp))
  )

;;===============================================================
;; Pretty printer
;;

(test-section "pretty printer")
(use gauche.pputil)
(test-module 'gauche.pputil)

(let ([data1 '(Lorem ipsum dolor sit amet consectetur adipisicing elit
               sed do eiusmod tempor incididunt ut labore et dolore)]
      [data2 '(Lorem (ipsum #(dolor (sit (amet . consectetur)))))]
      )
  (define (t name expect data . args)
    (test*/diff #"~|name| ~|args|" expect
                (with-output-to-string (^[] (apply pprint data args)))))
  (define elli (with-module gauche.internal (string-ellipsis)))
  (let-syntax
      ([t* (syntax-rules ()
             [(_ (data . args) expect)
              (t 'data expect data . args)])])

    (t* (data1)
        "(Lorem ipsum dolor sit amet consectetur adipisicing elit sed do eiusmod tempor\
       \n incididunt ut labore et dolore)\n")
    (t* (data1 :width #f)
        "(Lorem ipsum dolor sit amet consectetur adipisicing elit sed do eiusmod tempor incididunt ut labore et dolore)\n")
    (t* (data1 :width 40)
        "(Lorem ipsum dolor sit amet consectetur\
       \n adipisicing elit sed do eiusmod tempor\
       \n incididunt ut labore et dolore)\n")
    (t* (data1 :width 39)
        "(Lorem ipsum dolor sit amet consectetur\
       \n adipisicing elit sed do eiusmod tempor\
       \n incididunt ut labore et dolore)\n")
    (t* (data1 :width 38)
        "(Lorem ipsum dolor sit amet\
       \n consectetur adipisicing elit sed do\
       \n eiusmod tempor incididunt ut labore\
       \n et dolore)\n")
    (t* (data1 :length 5)
        #"(Lorem ipsum dolor sit amet ~elli)\n")
    (t* (data1 :length 1)
        #"(Lorem ~elli)\n")
    (t* (data1 :length 0)
        #"(~elli)\n")
    (t* (data1 :level 1 :length 5)
        #"(Lorem ipsum dolor sit amet ~elli)\n")
    (t* (data1 :level 0 :length 5)
        "#\n")
    (t* ('a :level 0)
        "a\n")

    (t* (data2 :level 0)
        "#\n")
    (t* (data2 :level 1)
        "(Lorem #)\n")
    (t* (data2 :level 2)
        "(Lorem (ipsum #))\n")
    (t* (data2 :level 3)
        "(Lorem (ipsum #(dolor #)))\n")
    (t* (data2 :level 4)
        "(Lorem (ipsum #(dolor (sit #))))\n")
    (t* (data2 :level 5)
        "(Lorem (ipsum #(dolor (sit (amet . consectetur)))))\n")
    (t* (data2 :level 4 :width 30)
        "(Lorem\
       \n (ipsum #(dolor (sit #))))\n")
    ))

;; Rounding error
;; https://github.com/shirok/Gauche/issues/941
(test*/diff "rounding"
            "(abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc
                     abc abc abc abc abc abc abc abc abc abc)
"
            (with-output-to-string (cut pprint (make-list 100 'abc)
                                        :indent 20 :width 60)))

;; some more rounding tests
(let ((expected '("(abc)\n"
                  "(abc abc)\n"
                  "(abc abc\n abc)\n"
                  "(abc abc\n abc abc)\n"
                  "(abc abc\n abc abc\n abc)\n"
                  "(abc abc\n abc abc\n abc abc)\n"
                  "(abc abc\n abc abc\n abc abc\n abc)\n"
                  "(abc abc\n abc abc\n abc abc\n abc abc)\n")))
  (dotimes [i (length expected)]
    (test*/diff #"rounding ~i"
                (~ expected i)
                (with-output-to-string
                  (cut pprint (make-list (+ i 1) 'abc) :width 8))))
  )

(test* "no newline" "(a\n a)"
       (call-with-output-string
         (cut pprint '(a a) :width 3 :newline #f :port <>)))

(test* "label (issue #484)" "#0=(a #1=(b #0# #1#))\n"
       (call-with-output-string
         (cut pprint '#0=(a #1=(b #0# #1#)) :port <>)))
(test* "label (issue #713)" "#0=(1 . #1=((2 . #1#) 3 . #0#))\n"
       (call-with-output-string
         (cut pprint '#0=(1 . #1=((2 . #1#) 3 . #0#)) :port <>)))

(test* "shorthand notation" "('a `b ,c ,@d `(e ,f 'g ,@h))\n"
       (call-with-output-string
         (cut pprint '('a `b ,c ,@d `(e ,f 'g ,@h)) :port <>)))
(test* "shorthand notation" "'#0=(a . '#0#)\n"
       (call-with-output-string
         (cut pprint ''#0=(a . '#0#) :port <>)))
(test* "shorthand notation" "'#0=(a ,@#1='(#0# . ,#1#))\n"
       (call-with-output-string
         (cut pprint ''#0=(a ,@#1='(#0# . ,#1#)) :port <>)))
(test* "shorthand notation" "#0=(a (quote . #0#))\n"
       (call-with-output-string
         (cut pprint '#0=(a (quote . #0#)) :port <>)))
(test* "shorthand notation" "#0=(a quote . #0#)\n"
       (call-with-output-string
         (cut pprint '#0=(a quote . #0#) :port <>)))
(test* "shorthand notation" "'(,@(the quick brown fox\
                           \n     jumps over the\
                           \n     ,@lazy dog))\n"
       (call-with-output-string
         (cut pprint ''(,@(the quick brown fox jumps over the ,@lazy dog))
              :port <> :width 25)))
(test* "shorthand notation" (test-none-of)
       (call-with-output-string
         (cut pprint `(,((with-module gauche.internal make-identifier)
                         'quote (current-module) '())
                       foo)
              :port <>)))

(let ((data (alist->hash-table
             (make-list 5 '(a . 1))
             (make-comparator #t (constantly #f) #f eq-hash))))
  (test* "recursive do-layout-elements"
         "#<hash-table custom[5]( (a . 1)\
        \n  (a . 1) (a . 1) (a . 1) (a . 1))>\n"
         (call-with-output-string
           (cut pprint data :width 35 :port <>)))
  (test* "recursive do-layout-elements"
         "#<hash-table custom[5]( (a . 1) (a . 1)\
        \n  (a . 1) (a . 1) (a . 1))>\n"
         (call-with-output-string
           (cut pprint data :width 45 :port <>)))
  )

;;===============================================================
;; utf-8 with BOM
;;

(test* "utf-8 with BOM" "foo!"
       (begin
         (load "data/utf-8-bom.scm" :paths `(,(build-path 'cld)))
         (foo)))

(test-end)
