;;
;; gauche.cgen.* tests
;;

(use gauche.test)
(use file.util)
(use gauche.cgen)

(test-start "gauche.cgen.*")

;;====================================================================
(test-section "gauche.cgen.unit")
(use gauche.cgen.unit)
(test-module 'gauche.cgen.unit)

;; simple things
(sys-unlink "tmp.o.c")
(test*/diff "cgen.unit basic stuff"
       "/* Generated by gauche.cgen */
static void foo(void);
static void foo() { ... }
void Scm__Init_tmp_2eo(void)
{
foo();
#if (defined(FOO))||(defined(BAR))
init_foo_bar();
#endif /* (defined(FOO))||(defined(BAR)) */
#if (defined(FOO))||(defined(BAR))
#if ((BAR_VERSION)>=(3))&&((FOO_VERSION)==(2))
some_trick();
#endif /* ((BAR_VERSION)>=(3))&&((FOO_VERSION)==(2)) */
#endif /* (defined(FOO))||(defined(BAR)) */
}
"
       (parameterize ([cgen-current-unit (make <cgen-unit> :name "tmp.o")])
         (cgen-init "foo();")
         (cgen-decl "static void foo(void);")
         (cgen-body "static void foo() { ... }")
         (cgen-with-cpp-condition '(or (defined FOO) (defined BAR))
           (cgen-init "init_foo_bar();")
           (cgen-with-cpp-condition '(and (>= BAR_VERSION 3) (== FOO_VERSION 2))
             (cgen-init "some_trick();")))
         (cgen-emit-c (cgen-current-unit))
         (begin0 (file->string "tmp.o.c")
           (sys-unlink "tmp.o.c"))))

;;====================================================================
(test-section "gauche.cgen.literal")
(use gauche.cgen.type)
(test-module 'gauche.cgen.literal)

;;====================================================================
(test-section "gauche.cgen.type")
(use gauche.cgen.type)
(test-module 'gauche.cgen.type)

;;====================================================================
(test-section "gauche.cgen.cise")
(use gauche.cgen.cise)
(test-module 'gauche.cgen.cise)

(let ()
  (define (t in out)
    (test* (format "canonicalize-vardecl ~s" in) out
           ((with-module gauche.cgen.cise canonicalize-vardecl) in)))

  (t '(a b c) '((a :: ScmObj) (b :: ScmObj) (c :: ScmObj)))
  (t '((a) (b) (c)) '((a) (b) (c)))
  (t '(a::x b::y (c::z)) '((a :: x) (b :: y) (c :: z)))
  (t '(a :: x b :: y (c :: z)) '((a :: x) (b :: y) (c :: z)))
  (t '(a:: x b ::y (c:: z)) '((a :: x) (b :: y) (c :: z)))
  (t '(a::(x y z) b::p) '((a :: (x y z)) (b :: p)))

  (t '((a::x init) (b::(x) init) (c :: x init))
     '((a :: x init) (b :: (x) init) (c :: x init)))
  (t '((a init) (b init) (c init))
     '((a init) (b init) (c init)))
  )

;; define-cfn
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'toplevel)))
  (define err (test-error))

  (c '(define-cfn) err)
  (c '(define-cfn a) err)
  (c '(define-cfn ()) err)
  (c '(define-cfn a ()) " ScmObj a(){{}}")
  (c '(define-cfn a () b) " ScmObj a(){{b;}}")
  (c '(define-cfn a () :static) "static ScmObj a(){{}}")
  (c '(define-cfn a () :inline) "inline ScmObj a(){{}}")
  (c '(define-cfn a () :static :inline) "static inline ScmObj a(){{}}")
  (c '(define-cfn a () :unknown) err)
  (c '(define-cfn a () ::foo) " foo a(){{}}")
  (c '(define-cfn a () ::(foo bar)) " foo bar a(){{}}")
  (c '(define-cfn a (b c::int)) " ScmObj a(ScmObj b,int c){{}}")
  (c '(define-cfn a (b::(const char*) ...)) " ScmObj a(const char* b,...){{}}")

  (c '(declare-cfn a ()) "")
  (c '(declare-cfn a () (return 0)) err))


;; define-cvar
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'toplevel)))

  (c '(define-cvar foo) " ScmObj foo;")
  (c '(define-cvar foo::int) " int foo;")
  (c '(define-cvar foo:: int) " int foo;")
  (c '(define-cvar foo :: int) " int foo;")
  (c '(define-cvar foo::(const char *)) " const char * foo;")
  (c '(define-cvar foo:: (.array int (10))) " int foo[10];")

  (c '(define-cvar foo 10) " ScmObj foo = 10;")
  (c '(define-cvar foo::int 10) " int foo = 10;")
  (c '(define-cvar foo:: int 10) " int foo = 10;")
  (c '(define-cvar foo :: int 10) " int foo = 10;")
  (c '(define-cvar foo::(const char *) NULL) " const char * foo = NULL;")
  (c '(define-cvar foo::int (+ 2 3)) " int foo = (2)+(3);")

  (c '(define-cvar foo :static 10) "static ScmObj foo = 10;")

  (c '(define-cvar foo::(.struct (name::(const char *)
                                  value::double)))
     " struct { const char * name; double value; } foo;")
  (c '(define-cvar foo::(.struct foostruct (name::(const char *)
                                            value::(.struct (r::double
                                                             i::double)))))
     " struct foostruct { const char * name; struct { double r; double i; } value; } foo;")
  (c '(define-cvar foo::(.union (d::double
                                 l::long)))
     " union { double d; long l; } foo;")
  (c '(define-cvar foo::(.struct foostruct))
     " struct foostruct foo;")
  (c '(define-cvar foo::(const .struct foostruct *))
     " const struct foostruct * foo;")
  (c '(define-cvar foo::(volatile .struct (a) **))
     " volatile struct { ScmObj a; } ** foo;")

  (c '(define-cvar foo::(.function (a::int b::char) ::long))
     " long (foo)(int a, char b);")
  (c '(define-cvar foo::(.function (a::int b::char) ::long *))
     " long (* foo)(int a, char b);")

  (c '(declare-cvar foo) "extern ScmObj foo;")
  (c '(declare-cvar foo::int) "extern int foo;")
  (c '(declare-cvar foo::int 10) (test-error))
  (c '(declare-cvar foo::int :static) (test-error))

  (c '(define-ctype foo::int) "typedef int foo;")
  (c '(define-ctype foo::(.struct (tag::ScmWord value attr)))
     "typedef struct { ScmWord tag; ScmObj value; ScmObj attr; } foo;")
  )

;; .define
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'toplevel)))

  (c '(.define foo) "#define foo\n")
  (c '(.define foo (+ 2 3)) "#define foo ((2)+(3))\n")
  (c '(.define foo (bar) (+ 2 3)) "#define foo(bar) ((2)+(3))\n")
  (c '(.define foo (a b) (+ a b)) "#define foo(a,b) ((a)+(b))\n"))

;; .if .cond .when .unless
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test*/diff (format "cise transform: ~a" form)
                exp
                (cise-render-to-string form 'toplevel)))

  (c '(.if foo then) '(""
                       "#if foo"
                       "then;"
                       "#endif /* foo */"))
  (c '(.if foo then else)
     '(""
       "#if foo"
       "then;"
       "#else /* !foo */"
       "else;"
       "#endif /* foo */"))
  (c '(.if (not (defined foo)) then)
     '(""
       "#if !(defined(foo))"
       "then;"
       "#endif /* !(defined(foo)) */"))
  (c '(.if (and (+ 1 2) (or (- 3 (<< 4 2)) 4)) then)
     '(""
       "#if ((1)+(2))&&(((3)-((4)<<(2)))||(4))"
       "then;"
       "#endif /* ((1)+(2))&&(((3)-((4)<<(2)))||(4)) */"))
  (c '(.when 1 foo bar)
     '(""
       "#if 1"
       "foo;"
       "bar;"
       "#endif /* 1 */"))
  (c '(.unless (defined haha) foo bar)
     '(""
       "#if !(defined(haha))"
       "foo;"
       "bar;"
       "#endif /* ! defined(haha) */"))

  (c '(.cond [(and foo bar) one]
             [(defined xyz) two]
             [else three])
     '(""
       "#if 0 /*dummy*/"
       "#elif (foo)&&(bar)"
       "one;"
       "#elif defined(xyz)"
       "two;"
       "#else"
       "three;"
       "#endif")))

;; statement-level tests
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define (t op exp0 exp1 exp2 exp3)
    (c (list op) exp0)
    (c (list op 'a) exp1)
    (c (list op 'a 'b) exp2)
    (c (list op 'a 'b 'c) exp3))
  (define err (test-error))

  ;; simple statement forms
  (t 'begin "{}" "{a;}" "{a;b;}" "{a;b;c;}")
  (t 'if err err "if (a){b;}" "if (a){b;} else {c;}")
  (c '(if a b c d) err)

  (t 'when err "if (a){{}}" "if (a){{b;}}" "if (a){{b;c;}}")
  (t 'unless err "if (!(a)){{}}" "if (!(a)){{b;}}" "if (!(a)){{b;c;}}")

  (t 'return "return;" "return (a);" err err)
  (t 'break "break;" err err err)
  (t 'continue "continue;" err err err)
  (t 'label err "a :; " err err)
  (t 'goto err "goto a;" err err)

  ;; some simple preprocessor directives
  ;; NB. all directives should start with "\n"
  (t '.if err err
     "\n#if a\nb;\n#endif /* a */\n"
     "\n#if a\nb;\n#else /* !a */\nc;\n#endif /* a */\n")
  (c '(.if a b c d) err)
  (t '.undef err "#undef a\n" err err)
  ;; (.include) should probably error out
  (t '.include ""
     "#include a\n"
     "#include a\n#include b\n"
     "#include a\n#include b\n#include c\n"))

;; let*
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (c '(let*) (test-error))
  (c '(let* ()) "{}")
  (c '(let* () c) "{c;}")
  (c '(let* (a) b) "{ScmObj a;b;}")
  (c '(let* (a::(b c)) d) "{b c a;d;}")
  (c '(let* ([a]) b) "{ScmObj a;b;}")
  (c '(let* ([a b]) c) "{ScmObj a=b;c;}")
  (c '(let* ([a] [b]) c) "{ScmObj a;ScmObj b;c;}")
  (c '(let* ([a]) b c) "{ScmObj a;b;c;}")
  (c '(let* ([a::]) b) (test-error))
  (c '(let* ([a::b]) c) "{b a;c;}")
  (c '(let* ([a::b c]) d) "{b a=c;d;}"))

;; cond
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define err (test-error))
  #;(c '(cond) err)
  (c '(cond) "")
  (c '(cond a) err)
  (c '(cond (a)) "if (a){}")
  (c '(cond (a b)) "if (a){b;}")
  (c '(cond (a b) (c d)) "if (a){b;}else if(c){d;}")
  (c '(cond (a b) (else c)) "if (a){b;} else {c;}")
  (c '(cond (a b) (else)) "if (a){b;} else {}")
  (c '(cond (a b) (c d) (else e))
     "if (a){b;}else if(c){d;} else {e;}")
  #;(c '(cond (else c)) err)
  (c '(cond (else c)) " else {c;}"))

;; case
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define err (test-error))
  (c '(case) err)
  (c '(case x) "switch (x) {}")
  (c '(case x a) err)
  (c '(case x (a)) err)
  (c '(case x [(a)])
     "switch (x) {case a : {break;}}")
  (c '(case x [(a) b])
     "switch (x) {case a : {b;break;}}")
  (c '(case x [(a) b] [(c) d])
     "switch (x) {case a : {b;break;}case c : {d;break;}}")
  (c '(case x [(a) b] (else c))
     "switch (x) {case a : {b;break;}default: {c;break;}}")
  (c '(case x [(a) b] (else))
     "switch (x) {case a : {b;break;}default: {break;}}")
  (c '(case x [(a) b] [(c) d] (else e))
     "switch (x) {case a : {b;break;}case c : {d;break;}default: {e;break;}}")
  (c '(case x (else c))
     "switch (x) {default: {c;break;}}"))

;; case/fallthrough
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define err (test-error))
  (c '(case/fallthrough) err)
  (c '(case/fallthrough x) "switch (x) {}")
  (c '(case/fallthrough x a) err)
  (c '(case/fallthrough x (a)) err)
  (c '(case/fallthrough x [(a)])
     "switch (x) {case a : {}/*FALLTHROUGH*/}")
  (c '(case/fallthrough x [(a) b])
     "switch (x) {case a : {b;}/*FALLTHROUGH*/}")
  (c '(case/fallthrough x [(a) b] [(c) d])
     "switch (x) {case a : {b;}/*FALLTHROUGH*/case c : {d;}/*FALLTHROUGH*/}")
  (c '(case/fallthrough x [(a) b] (else c))
     "switch (x) {case a : {b;}/*FALLTHROUGH*/default: {c;}/*FALLTHROUGH*/}")
  (c '(case/fallthrough x [(a) b] (else))
     "switch (x) {case a : {b;}/*FALLTHROUGH*/default: {}/*FALLTHROUGH*/}")
  (c '(case/fallthrough x [(a) b] [(c) d] (else e))
     "switch (x) {case a : {b;}/*FALLTHROUGH*/case c : {d;}/*FALLTHROUGH*/default: {e;}/*FALLTHROUGH*/}")
  (c '(case/fallthrough x (else c))
     "switch (x) {default: {c;}/*FALLTHROUGH*/}"))

;; for/loop
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define err (test-error))
  (c '(for) err)
  (c '(for ()) "for (;;){}")
  (c '(for () a) "for (;;){a;}")
  (c '(for (a)) err)
  (c '(for (a b)) err)
  (c '(for (a b c)) "for (a; b; c){}")
  (c '(for (a b c d)) err)
  (c '(loop) "for (;;){}")
  (c '(loop a) "for (;;){a;}"))

;; while
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define err (test-error))
  (c '(while) err)
  (c '(while a) "while(a){}")
  (c '(while a b) "while(a){b;}")
  (c '(while a b c) "while(a){b;c;}"))

;; dotimes
(parameterize ([cise-emit-source-line #f])
  (define (next-gensym-counter)
    (let1 sym (symbol->string (gensym ""))
      (+ (string->number sym) 1)))
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define err (test-error))
  (c '(dotimes) err)
  (c '(dotimes ()) err)
  (c '(dotimes (a)) err)
  (let1 sym (format "cise__~a" (next-gensym-counter))
    (c '(dotimes (a b))
       (format "{int a=0;int ~a=b;for (; (a)<(~a); (a)++){}}" sym sym))))

;; expression-level tests
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'expr)))
  (define err (test-error))
  (define (t op exp0 exp1 exp2 exp3)
    (c (list op) exp0)
    (c (list op 'a) exp1)
    (c (list op 'a 'b) exp2)
    (c (list op 'a 'b 'c) exp3))

  #;(t '+ "(0)" "(a)" "(a)+(b)" "((a)+(b))+(c)")
  (t '+ err "+(a)" "(a)+(b)" "((a)+(b))+(c)")
  (t '- err "-(a)" "(a)-(b)" "((a)-(b))-(c)")
  ;; (* a) is pointer dereference, not multiplication
  ;; (*) should probably return 1
  (t '* err "*(a)" "(a)*(b)" "((a)*(b))*(c)")
  #;(t '/ err "1.0/(a)" "(a)/(b)" "((a)/(b))/(c)")
  (t '/ err "/(a)" "(a)/(b)" "((a)/(b))/(c)")
  (t '% err err "(a)%(b)" err)

  #;(t 'and err "(a)" "(a)&&(b)" "((a)&&(b))&&(c)")
  (t 'and err "&&(a)" "(a)&&(b)" "((a)&&(b))&&(c)")
  #;(t 'or err "(a)" "(a)||(b)" "((a)||(b))||(c)")
  (t 'or err "||(a)" "(a)||(b)" "((a)||(b))||(c)")
  (t 'not err "!(a)" err err)

  #;(t 'logand err err "(a)&(b)" "((a)&(b))&(c)")
  (t 'logand err "&(a)" "(a)&(b)" "((a)&(b))&(c)")
  #;(t 'logior err err "(a)|(b)" "((a)|(b))|(c)")
  (t 'logior err "|(a)" "(a)|(b)" "((a)|(b))|(c)")
  #;(t 'logxor err err "(a)^(b)" "((a)^(b))^(c)")
  (t 'logxor err "^(a)" "(a)^(b)" "((a)^(b))^(c)")
  (t 'lognot err "~(a)" err err)

  (t '& err "&(a)" err err)

  (t 'pre++ err "++(a)" err err)
  (t 'pre-- err "--(a)" err err)
  (t 'post++ err "(a)++" err err)
  (t 'post-- err "(a)--" err err)

  (t '< err err "(a)<(b)" err)
  (t '<= err err "(a)<=(b)" err)
  (t '> err err "(a)>(b)" err)
  (t '>= err err "(a)>=(b)" err)
  (t '== err err "(a)==(b)" err)
  (t '!= err err "(a)!=(b)" err)

  (t '<< err err "(a)<<(b)" err)
  (t '>> err err "(a)>>(b)" err)

  #;(t 'set! err err "a=(b)" err)
  (t 'set! "" err "a=(b)" err)
  (c '(set! a b c d) "a=(b),c=(d)")
  (c '(set! a b c d e) err)
  (c '(set! a b c d e f) "a=(b),c=(d),e=(f)")

  ;; NB. are parentheses around 'a' necessary? "=" assignments do not
  ;; have them
  (t '+= err err "(a)+=(b)" err)
  (t '-= err err "(a)-=(b)" err)
  (t '*= err err "(a)*=(b)" err)
  (t '/= err err "(a)/=(b)" err)
  (t '%= err err "(a)%=(b)" err)
  (t '>>= err err "(a)>>=(b)" err)
  (t '<<= err err "(a)<<=(b)" err)
  (t 'logand= err err "(a)&=(b)" err)
  (t 'logior= err err "(a)|=(b)" err)
  (t 'logxor= err err "(a)^=(b)" err)

  #;(t '-> err err "(a)->b" "(a)->b->c")
  (t '-> err "(a)->" "(a)->b" "(a)->b->c")
  #;(t 'ref err err "(a).b" "(a).b.c")
  (t 'ref err "(a)." "(a).b" "(a).b.c")
  #;(t 'aref err err "(a)[b]" "(a)[b][c]")
  (t 'aref err "(a)" "(a)[b]" "(a)[b][c]")

  (t 'cast err err "((a )(b))" err)
  (t '.type err "a " "a b " "a b c ")

  (t '?: err err err "((a)?(b):(c))")
  (c '(?: a b c d) err))

#;(test* "operator expressions should be rejected at toplevel"
       (test-error)
       (cise-render-to-string '(< a b) 'toplevel))

;; result
(parameterize ([cise-emit-source-line #f])
  (define (c form exp)
    (test* (format "cise transform: ~a" form) exp
           (cise-render-to-string form 'stmt)))
  (define err (test-error))
  (c '(result) err)
  (c '(result e) "SCM_RESULT=(e);")
  (c '(result e0 e1) "SCM_RESULT0=(e0),SCM_RESULT1=(e1);")
  (c '(result e0 e1 e2) "SCM_RESULT0=(e0),SCM_RESULT1=(e1),SCM_RESULT2=(e2);")
  (c '(result e0 e1 e2 e3) "SCM_RESULT0=(e0),SCM_RESULT1=(e1),SCM_RESULT2=(e2),SCM_RESULT3=(e3);"))

;;====================================================================
(test-section "gauche.cgen.stub")
(use gauche.cgen.stub)
(test-module 'gauche.cgen.stub)

;; TODO: We need comprehensive tests, but for now, we have specific tests
;; for particularly nasty features.

;; cise-ambient for stub compilation
(let ([c (lambda (form exp)
           (test* (format "cise transform: ~a" form) exp
                  (cise-render-to-string form 'stmt)))])
  (parameterize ([cise-emit-source-line #f]
                 [cise-ambient (cgen-stub-cise-ambient (cise-ambient))])
    (c '(return) "goto SCM_STUB_RETURN;")
    (c '(return e) "{SCM_RESULT=(e);goto SCM_STUB_RETURN;}")
    (c '(return e0 e1) "{SCM_RESULT0=(e0),SCM_RESULT1=(e1);goto SCM_STUB_RETURN;}")
    (c '(return e0 e1 e2) "{SCM_RESULT0=(e0),SCM_RESULT1=(e1),SCM_RESULT2=(e2);goto SCM_STUB_RETURN;}")
    (c '(return e0 e1 e2 e3) "{SCM_RESULT0=(e0),SCM_RESULT1=(e1),SCM_RESULT2=(e2),SCM_RESULT3=(e3);goto SCM_STUB_RETURN;}")))

;; generate stub and compare with pre-generated file
(let ()
  (define (test-stub-output name . stub-forms)
    ($ test-with-temporary-directory "test.o"
       (^[]
         ($ test*/diff #"stub-output ~name"
            ;; relative to this file
            `(content-of ,(build-path "data" "cgen" name))
            (let* ([out (build-path "test.o" name)]
                   [out.stub (path-swap-extension out "stub")]
                   [out.c (path-swap-extension out "c")])
              (with-output-to-file out.stub
                (^[] (dolist [form stub-forms]
                       (write form)
                       (newline))))
              (cgen-genstub out.stub :output-directory "test.o")
              (file->string (sys-normalize-pathname out.c :absolute #t)))))))

  ($ test-stub-output "cclass-basic"
     '(define-cclass foo :base :private :no-meta
        "Foo*" "FooClass"
        (c "SCM_CLASS_TOP_CPL")
        ((x) (y) (z))))
  ($ test-stub-output "cclass-metaclass"
     '(define-cclass foo :base :private
        "Foo*" "FooClass"
        (c "SCM_CLASS_TOP_CPL")
        ()
        (metaclass "SCM_CLASS_CLASS")))
  ($ test-stub-output "cclass-metaclass2"
     '(define-cclass foo :base :private
        "Foo*" "FooClass"
        (c "SCM_CLASS_TOP_CPL")
        ()
        (metaclass <class>)))
  )

;;====================================================================
(test-section "gauche.cgen.precomp")
(use gauche.cgen.precomp)
(test-module 'gauche.cgen.precomp)

;;====================================================================
(test-section "gauche.cgen.standalone")
(use gauche.cgen.standalone)
(test-module 'gauche.cgen.standalone)

;;====================================================================
(test-section "gauche.cgen.bbb")
(use gauche.cgen.bbb)
(test-module 'gauche.cgen.bbb)

;;====================================================================
(test-section "gauche.cgen")
(use gauche.cgen)
(test-module 'gauche.cgen)

(test-end)
