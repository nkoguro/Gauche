(use gauche.test)
(use gauche.config)
(use gauche.ffitest)
(use util.match)
(use file.util)

(cond-expand
 [gauche.os.windows (exit 0)]
 [else
  ;; Currently we only support those platforms
  (unless (#/^x86_64-.*(-linux|-darwin)/ (gauche-config "--arch"))
    (exit 0))])

(test-start "ffitest")
(use lang.asm.x86_64)

;;==========================================================================
(test-section "raw call")

(define (foreign-call dlo name args rettype)
  (parameterize ([(with-module gauche.typeutil native-ptr-fill-enabled?) #t])
    ((with-module gauche.internal call-amd64)
     (dlobj-get-entry-address dlo name)
     args rettype)))

(define (test-foreign-call dlo name expected args rettype)
  (test* #"call ~name" expected
         (foreign-call dlo name args rettype)))

(let ((dlo (dynamic-load "gauche--ffitest" :init-function #f)))
  (test* "open dlo" #t (is-a? dlo <dlobj>))
  (let ((dle (dlobj-get-entry-address dlo "_f_v")))
    (test* "get native-handle" #t (is-a? dle <native-handle>))
    (test* "call f_o" (list (undefined) "it works")
           (let* ((r #f)
                  (s (with-output-to-string
                       (^[]
                         (parameterize ([(with-module gauche.typeutil
                                           native-ptr-fill-enabled?) #t])
                           (set! r ((with-module gauche.internal call-amd64)
                                    dle '() <void>)))))))
             (list r s))))

  (test-section "simple, register passing call (integral)")
  (test-foreign-call dlo "_f_o" 'it_works '() <top>)
  (test-foreign-call dlo "_f_i" 42 '() <intptr_t>)
  (test-foreign-call dlo "_f_s" "it works" '() <c-string>)

  (test-foreign-call dlo "_fo_o" '(wow . huh) `((,<top> wow)) <top>)
  (test-foreign-call dlo "_fi_o" '(7 . huh) `((,<intptr_t> 6)) <top>)
  (test-foreign-call dlo "_fi_o" '(-9 . huh) `((,<intptr_t> -10)) <top>)
  (test-foreign-call dlo "_fs_o" 5 `((,<c-string> "hello")) <top>)
  (test-foreign-call dlo "_fo_i" 3 `((,<top> (a b c))) <intptr_t>)
  (test-foreign-call dlo "_fi_i" 121 `((,<intptr_t> 11)) <intptr_t>)
  (test-foreign-call dlo "_fs_i" 6 `((,<c-string> "gauche")) <intptr_t>)
  (test-foreign-call dlo "_fo_s" "(a b c)" `((,<top> (a b c))) <c-string>)

  (test-foreign-call dlo "_foo_o" '(a . b) `((,<top> a) (,<top> b)) <top>)
  (test-foreign-call dlo "_foi_o" '(a . 1) `((,<top> a) (,<intptr_t> 0)) <top>)
  (test-foreign-call dlo "_fis_i" (char->integer #\c) `((,<intptr_t> 2) (,<c-string> "abcde")) <intptr_t>)

  (test-foreign-call dlo "_fois_o" '("foo" 100 (cent))
                     `((,<top> (cent)) (,<intptr_t> 100) (,<c-string> "foo")) <top>)
  (test-foreign-call dlo "_foiso_o" '(3+2i "foo" 100 (cent))
                     `((,<top> (cent)) (,<intptr_t> 100) (,<c-string> "foo") (,<top> 3+2i)) <top>)
  (test-foreign-call dlo "_foisoi_o" '(-56789 3+2i "foo" 100 (cent))
                     `((,<top> (cent)) (,<intptr_t> 100) (,<c-string> "foo") (,<top> 3+2i) (,<intptr_t> -56789))
                     <top>)
  (test-foreign-call dlo "_foisois_o" '("" -56789 3+2i "foo" 100 (cent))
                     `((,<top> (cent)) (,<intptr_t> 100) (,<c-string> "foo") (,<top> 3+2i) (,<intptr_t> -56789) (,<c-string> ""))
                     <top>)

  (test-section "register passing (flonum)")
  (test-foreign-call dlo "_fd_o" 101.0  `((,<double> 100.0)) <top>)
  (test-foreign-call dlo "_fid_o" 99.0  `((,<intptr_t> 100) (,<double> 1.0)) <top>)
  (test-foreign-call dlo "_fdi_o" 99.0  `((,<double> 100.0) (,<intptr_t> 1)) <top>)
  (test-foreign-call dlo "_fiiiiii_d" 10.5
                     `((,<intptr_t> 1) (,<intptr_t> 2) (,<intptr_t> 3) (,<intptr_t> 4) (,<intptr_t> 5) (,<intptr_t> 6)) <double>)

  (test-section "calling back to Scheme")
  (test-foreign-call dlo "_fo_o_cb" '(z . z) `((,<top> z)) <top>)
  (test-foreign-call dlo "_foo_o_cb" '(d c b a)
                     `((,<top> ,reverse) (,<top> (a b c d))) <top>)
  (test-foreign-call dlo "_foo_o_cb"
                     (test-error <error> "list is supposed to be of type list, but got zzz")
                     `((,<top> ,reverse) (,<top> zzz)) <top>)

  (test-section "spill-to-stack case")
  (test-foreign-call dlo "_fooooooo_o" '((a b c d e) (f g))
                     `((,<top> a) (,<top> b) (,<top> c) (,<top> d) (,<top> e) (,<top> f) (,<top> g)) <top>)
  (test-foreign-call dlo "_foooooooo_o" '((a b c d e) (f g h))
                     `((,<top> a) (,<top> b) (,<top> c) (,<top> d) (,<top> e) (,<top> f) (,<top> g) (,<top> h)) <top>)
  (test-foreign-call dlo "_fooooooooi_o" '((a b c d e) (f g h 5))
                     `((,<top> a) (,<top> b) (,<top> c) (,<top> d) (,<top> e) (,<top> f) (,<top> g) (,<top> h) (,<intptr_t> 4))
                     <top>)
  (test-foreign-call dlo "_fdddddddddd_d"
                     (* (+ 1.0 1.1 1.2 1.3 1.4) (+ 1.5 1.6 1.7 1.8 1.9))
                     `((,<double> 1.0) (,<double> 1.1) (,<double> 1.2) (,<double> 1.3) (,<double> 1.4)
                       (,<double> 1.5) (,<double> 1.6) (,<double> 1.7) (,<double> 1.8) (,<double> 1.9))
                     <double>)
  (test-foreign-call dlo "_fiiiiiiddddddddidid_d"
                     (* (+ 1 2 3 4 5 6 7 8)
                        (+ 1.0 1.1 1.2 1.3 1.4
                           1.5 1.6 1.7 1.8 1.9))
                     `((,<intptr_t> 1) (,<intptr_t> 2) (,<intptr_t> 3) (,<intptr_t> 4) (,<intptr_t> 5) (,<intptr_t> 6)
                       (,<double> 1.0) (,<double> 1.1) (,<double> 1.2) (,<double> 1.3)
                       (,<double> 1.4) (,<double> 1.5) (,<double> 1.6) (,<double> 1.7)
                       (,<intptr_t> 7) (,<double> 1.8) (,<intptr_t> 8) (,<double> 1.9))
                     <double>)

  (test-foreign-call dlo "_fooooooooooo_o_cb"
                     '(A B C D E F G H I . J)
                     `((,<top> ,list*) (,<top> A) (,<top> B) (,<top> C) (,<top> D) (,<top> E)
                       (,<top> F) (,<top> G) (,<top> H) (,<top> I) (,<top> J))
                     <top>)

  (test-foreign-call dlo "_fio_var_o"
                     '(A A B B C C)
                     `((,<intptr_t> 3) (,<top> A) (,<top> B) (,<top> C))
                     <top>)
  (test-foreign-call dlo "_fio_var_o"
                     '(A A B B C C D D E E F F G G H H)
                     `((,<intptr_t> 8) (,<top> A) (,<top> B) (,<top> C) (,<top> D) (,<top> E) (,<top> F) (,<top> G) (,<top> H))
                     <top>)
  (test-foreign-call dlo "_fido_var_o"
                     '(A A B B C C D D 1.0)
                     `((,<intptr_t> 4) (,<double> 1.0) (,<top> A) (,<top> B) (,<top> C) (,<top> D))
                     <top>)

  (test-section "ensure error frees codepad memory")
  (test* "error and codepad memory management" #t
         (let1 proc (lambda (_) (error "wow"))
           (dotimes [2000]
             (guard (e [else #t])
               (foreign-call dlo "_foo_o_cb" `((,<top> ,proc) (,<top> #f)) <top>)))
           #t))
  )

(test-end)
