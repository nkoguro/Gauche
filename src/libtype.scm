;;;
;;; libtype.scm - type-related stuff
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

;; This must be the first form to prevents generation of *.sci file
(declare (keep-private-macro define-type-constructor))

;; In Gauche, types are a data structure that appears in both compile-time
;; and run-time, describes metalevel properties of run-time data.
;;
;; Gauche has two kinds of types--generative types and descriptive types.
;; Generative types are the types that are actually used to generate the
;; actual data---we also call it classes.  Descriptive types are, otoh,
;; used only to descrive the nature of data at the certain point of program
;; execution---for example, you may say the argument must be either <integer>
;; or <boolean>.  The descriptive type can't be used to generate an instance,
;; only to be used to validate and/or infer the actual (generative) type of
;; data.
;;
;; Descriptive types can be constructed by type constructors.  Since the
;; compiler needs to know about types, type constructor expressions
;; are evaluated at the compile time, much like macros.
;;
;; We could implemented types with macros, but it would be tricky.  Unlike
;; macros, type expression needs to evaluate from inside to outside, just
;; like the ordinary expression.  It's more like compile-time constant folding.
;;
;; Since type handing is deeply intertwined with compiler, we let the compiler
;; recognize type expression specifically, rather than reusing existing
;; evaluation mechanism.  When the compile sees (C x ...) and C has an
;; inlineable binding to an instance of <type-constructor-meta>, it recognizes
;; type expression.

;; This module is not meant to be `use'd.   It is just to hide
;; auxiliary procedures from the rest of the system.  The necessary
;; bindings are injected into 'gauche' module at the initialization time.
(define-module gauche.typeutil)
(select-module gauche.typeutil)
(use util.match)

;; Metaclass: <type-constructor-meta>
;;   Instance classes of this metaclass are used to create an abstract types.
(inline-stub
 (.include "gauche/priv/classP.h")
 (define-ctype ScmTypeConstructor
   ::(.struct ScmTypeConstructorRec
              (common::ScmClass
               constructor::ScmObj
               deconstructor::ScmObj
               validator::ScmObj)))

 ;; constructor - a procedure to build a descriptive type, an instance
 ;;               of the metaclass.
 ;; deconstructor - returns a list of objects which, when passed to the
 ;;               constructor, recreates the type.
 ;;               each element must be either a simple constant or a type.
 ;; validator - takes type and obj, returns if obj is valid as type.
 (define-cclass <type-constructor-meta> :base :private :no-meta
   "ScmTypeConstructor*" "Scm_TypeConstructorMetaClass"
   (c "SCM_CLASS_METACLASS_CPL")
   ((constructor)
    (deconstructor)
    (validator))
   )

 (define-cfn Scm_TypeConstructorP (klass) ::int
   (return (SCM_ISA klass (& Scm_TypeConstructorMetaClass))))

 (define-ctype ScmDescriptiveType
   ::(.struct ScmDescriptiveTypeRec
              (hdr::ScmInstance
               name::ScmObj)))

 (define-cclass <descriptive-type> :base :private :no-meta
   "ScmDescriptiveType*" "Scm_DescriptiveTypeClass"
   (c "SCM_CLASS_METACLASS_CPL+1")
   ((name))
   (allocator (let* ([z::ScmDescriptiveType*
                      (SCM_NEW_INSTANCE ScmDescriptiveType klass)])
                (cast void initargs)    ;suppress unused warning
                (set! (-> z name) SCM_FALSE)
                (return (SCM_OBJ z)))))

 (define-ctype ScmStubType
   ::(.struct ScmStubTypeRec
              (hdr::ScmHeader
               name::ScmObj
               c-of-type::(.function (obj) ::int *)
               of-type::ScmObj))) ; obj -> bool

 (define-cclass <stub-type> :built-in :private :no-meta
   "ScmStubType*" "Scm_StubTypeClass"
   (c "SCM_CLASS_METACLASS_CPL+1")
   ((name)
    (of-type))
   (printer (Scm_Printf port "#<stub-type %S>" (-> (SCM_STUB_TYPE obj) name))))

 ;; (of-type? ofj type)
 ;;    This may push C continuations on VM, so must be called on VM.
 (define-cfn Scm_VMOfType (obj type)
   (cond [(SCM_PROXY_TYPE_P type)
          (return (Scm_VMIsA obj (Scm_ProxyTypeRef (SCM_PROXY_TYPE type))))]
         [(SCM_DESCRIPTIVE_TYPE_P type)
          (let* ([k::ScmClass* (SCM_CLASS_OF type)])
            (SCM_ASSERT (SCM_TYPE_CONSTRUCTOR_META_P k))
            (return (Scm_VMApply2 (-> (SCM_TYPE_CONSTRUCTOR_META k) validator)
                                  type obj)))]
         [(SCM_STUB_TYPE_P type)
          (if (-> (SCM_STUB_TYPE type) c-of-type)
            (return (SCM_MAKE_BOOL
                     (funcall (-> (SCM_STUB_TYPE type) c-of-type) obj)))
            (return (Scm_VMApply1 (-> (SCM_STUB_TYPE type) of-type) obj)))]
         [(SCM_CLASSP type)
          (return (Scm_VMIsA obj (SCM_CLASS type)))]
         [else
          (Scm_Error "Second argument of of-type? must be a type, but got: %S"
                     type)]))

 (define-cproc of-type? (obj type) Scm_VMOfType)
 )

;; define-type-constructor name supers
;;   (slot ...)
;;   of-type

(define-syntax define-type-constructor
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ name supers slots constructor deconstructor validator)
        (let ([meta-name (rxmatch-if (#/^<(.*)>$/ (symbol->string name))
                             [_ trimmed]
                           (string->symbol #"<~|trimmed|-meta>")
                           (string->symbol #"~|name|-meta"))]
              [supers (if (null? supers)
                        (list (r'<descriptive-type>))
                        supers)])
          (quasirename r
            `(begin
               (define-class ,meta-name (<type-constructor-meta>) ())
               (define-class ,name ,supers ,slots
                 :metaclass ,meta-name
                 :constructor ,constructor
                 :deconstructor ,deconstructor
                 :validator ,validator))))]))))

(define-method allocate-instance ((t <descriptive-type>) initargs)
  (error "Abstract type instance cannot instantiate a concrete object:" t))

(define-method write-object ((t <descriptive-type>) port)
  (format port "#~a" (~ t'name)))

;; Equality is used when consolidate literals.  It's not lightweight
;; (it calls deconstructor, which allocates).
(define-method object-equal? ((x <descriptive-type>) (y <descriptive-type>))
  (and (equal? (class-of x) (class-of y))
       (equal? (deconstruct-type x) (deconstruct-type y))))

;; Internal API, required to precompile descriptive type constant
(define-method deconstruct-type ((t <descriptive-type>))
  ((~ (class-of t)'deconstructor) t))

;; This is called from initialization of precompiled code to recove
;; descripitve type instance.
(inline-stub
 (define-cfn Scm_ConstructType (ctor args)
   (unless (Scm_TypeConstructorP ctor)
     (SCM_TYPE_ERROR ctor "<type-constructor-meta>"))
   (return (Scm_ApplyRec (-> (cast ScmTypeConstructor* ctor) constructor)
                         args)))
 )

;;;
;;; Proxy type API.
;;;

;; These are available in gauche.internal

(define-cproc wrap-with-proxy-type (id gloc)
  (unless (SCM_IDENTIFIERP id)
    (SCM_TYPE_ERROR id "identifier"))
  (unless (SCM_GLOCP gloc)
    (SCM_TYPE_ERROR gloc "gloc"))
  (return (Scm_MakeProxyType (SCM_IDENTIFIER id) (SCM_GLOC gloc))))

(define-cproc proxy-type-ref (type)
  (unless (SCM_PROXY_TYPE_P type)
    (SCM_TYPE_ERROR type "proxy-type"))
  (return (SCM_OBJ (Scm_ProxyTypeRef (SCM_PROXY_TYPE type)))))

(define-cproc proxy-type-id (type)
  (unless (SCM_PROXY_TYPE_P type)
    (SCM_TYPE_ERROR type "proxy-type"))
  (return (Scm_ProxyTypeId (SCM_PROXY_TYPE type))))

;;;
;;; Utilities
;;;

(define (join-class-names classes)
  (string-join (map (^k (x->string
                         (cond [(is-a? k <class>) (class-name k)]
                               [(is-a? k <descriptive-type>) (~ k'name)]
                               [(is-a? k <proxy-type>)
                                (~ (proxy-type-id k) 'name)]
                               [else k])))
                    classes)
               " " 'prefix))

(define (make-compound-type-name op-name classes)
  ($ string->symbol
     $ string-append "<" (x->string op-name) (join-class-names classes) ">"))

(define (make-min-max-len-type-name op-name classes min max)
  ($ string->symbol
     $ string-append "<" (x->string op-name) (join-class-names classes)
     (if min
       (if max
         (if (= min max)
           (format " ~d" min)
           (format " ~d..~d" min max))
         (format " ~d.." min))
       (if max
         (format " ..~d" max)
         ""))
     ">"))

;;;
;;; Class: <^>  (maybe we want to name it <λ>)
;;;   Creates a procedure type.
;;;   The signature can be specified as
;;;
;;;       <argtype1> <argtype2> ... :- <rettype1> <rettype2> ...
;;;
;;;   Argument types and/or return types can be also a single symbol '*,
;;;   indicating arbitrary number of args/values.   That is, any procedure
;;;   can be of type '* :- '*.
;;;
;;;   NB: Currently we don't keep the return value info in procedure, so
;;;   we only allow "wild card" '* as the results.
;;;
;;;   TODO: How to type optional, keyword and rest arguments?
;;;


(define (make-^ . rest)
  (define (scan-args xs as)
    (match xs
      [() (error "Missing ':-' in the procedure type constructor arguments:"
                 rest)]
      [(':- . xs) (scan-results xs (reverse as) '())]
      [('* ':- . xs)
       (if (null? as)
         (scan-results xs '* '())
         (error "Invalid '* in the procedure type constructor arguments:"
                rest))]
      [else
       (if (is-a? (car xs) <type>)
         (scan-args (cdr xs) (cons (car xs) as))
         (error "Non-type argument in the procedure type constructor:"
                (car xs)))]))
  (define (scan-results xs args rs)
    (cond [(null? xs) (values args (reverse rs))]
          [(and (null? rs) (eq? (car xs) '*) (null? (cdr xs)))
           (values args '*)]
          [(is-a? (car xs) <type>)
           (scan-results (cdr xs) args (cons (car xs) rs))]
          [else
           (error "Non-class argument in the procedure type constructor:"
                  (car xs))]))

  (receive (args results) (scan-args rest '())
    (unless (eq? results '*)
      (error "Result type must be '*, for we don't support result type checking \
              yet:" results))
    (make <^>
      :name (make-compound-type-name '^ rest)
      :arguments args
      :results results)))

(define (deconstruct-^ type)
  (if (eq? (~ type'results) '*)
    (append (~ type'arguments) '(:- *))
    (append (~ type'arguments) '(:-) (~ type'results))))

(define (validate-^ type obj)
  (if (eq? (~ type'arguments) '*)
    (or (is-a? obj <procedure>)
        (is-a? obj <generic>)
        (let1 k (class-of obj)
          (let loop ([ms (~ object-apply'methods)])
            (cond [(null? ms) #f]
                  [(subtype? k (car (~ (car ms)'specializers)))]
                  [else (loop (cdr ms))]))))
    (apply applicable? obj (~ type'arguments))))

(define-type-constructor <^> ()
  ((arguments :init-keyword :arguments)
   (results :init-keyword :results))
  make-^
  deconstruct-^
  validate-^)

;;;
;;; Class: </>
;;;   Creates a union type.
;;;

(define (make-/ . args)
  (assume (every (cut is-a? <> <type>) args))
  (make </>
    :name (make-compound-type-name '/ args)
    :members args))

(define (deconstruct-/ type)
  (~ type'members))

(define (validate-/ type obj)
  (any (cut of-type? obj <>) (~ type'members)))

(define-type-constructor </> ()
  ((members :init-keyword :members))
  make-/
  deconstruct-/
  validate-/)

;;;
;;; Class: <?>
;;;   Creates a boolean-optional type, that is, <type> or #f.
;;;

(define (make-? ptype)
  (assume (is-a? ptype <type>))
  (make <?>
    :name (make-compound-type-name '? `(,ptype))
    :primary-type ptype))

(define (deconstruct-? type)
  (list (~ type'primary-type)))

(define (validate-? type obj)
  (or (eqv? obj #f) (of-type? obj (~ type'primary-type))))

(define-type-constructor <?> ()
  ((primary-type :init-keyword :primary-type))
  make-?
  deconstruct-?
  validate-?)

;;;
;;; Class: <Tuple>
;;;   Fixed-lenght list, each element having its own type constraints.
;;;

(define (make-Tuple . args)
  (assume (every (cut is-a? <> <type>) args))
  (make <Tuple>
    :name (make-compound-type-name 'Tuple args)
    :elements args))

(define (deconstruct-Tuple type)
  (~ type'elements))

(define (validate-Tuple type obj)
  (let loop ((obj obj) (elts (~ type'elements)))
    (if (null? obj)
      (null? elts)
      (and (pair? obj)
           (pair? elts)
           (of-type? (car obj) (car elts))
           (loop (cdr obj) (cdr elts))))))

(define-type-constructor <Tuple> ()
  ((elements :init-keyword :elements))
  make-Tuple
  deconstruct-Tuple
  validate-Tuple)

;;;
;;; Class: <List>
;;;   A list of specified types.
;;;

(define (make-List etype :optional (min #f) (max #f))
  (make <List>
    :name (make-min-max-len-type-name 'List (list etype) min max)
    :element-type etype
    :min-length min
    :max-length max))

(define (deconstruct-List type)
  (list (~ type'element-type) (~ type'min-length) (~ type'max-length)))

(define (validate-List type obj)
  (let ([et (~ type'element-type)]
        [mi (~ type'min-length)]
        [ma (~ type'max-length)])
    (if (not (or mi ma))
      ;; simple case
      (let loop ([obj obj])
        (cond [(null? obj) #t]
              [(not (pair? obj)) #f]
              [(of-type? (car obj) et) (loop (cdr obj))]
              [else #f]))
      ;; general case
      (let loop ([obj obj] [n 0])
        (cond [(null? obj) (or (not mi) (<= mi n))]
              [(and ma (<= ma n)) #f]
              [(not (pair? obj)) #f]
              [(of-type? (car obj) et) (loop (cdr obj) (+ n 1))]
              [else #f])))))

(define-type-constructor <List> ()
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f))
  make-List
  deconstruct-List
  validate-List)

;;;
;;; <Vector> element-type [min-length [max-length]]
;;;

(define (make-Vector etype :optional (min #f) (max #f))
  (make <Vector>
    :name (make-min-max-len-type-name 'Vector (list etype) min max)
    :element-type etype
    :min-length min
    :max-length max))

(define (deconstruct-Vector type)
  (list (~ type'element-type) (~ type'min-length) (~ type'max-length)))

(define (validate-Vector type obj)
  (and (vector? obj)
       (let ([et (~ type'element-type)]
             [mi (~ type'min-length)]
             [ma (~ type'max-length)]
             [len (vector-length obj)])
         (and (or (not mi) (<= mi len))
              (or (not ma) (<= len ma))
              (let loop ([i 0])
                (cond [(= i len) #t]
                      [(of-type? (vector-ref obj i) et) (loop (+ i 1))]
                      [else #f]))))))

(define-type-constructor <Vector> ()
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f))
  make-Vector
  deconstruct-Vector
  validate-Vector)

;;;
;;; Types for stubs
;;;

;; Each of these types has a corresponding cgen-type that maintains
;; the knowledge how it is represented in C.

(inline-stub
 (define-cfn Scm_MakeStubType (name::(const char*)
                               c-of-type::(.function (obj)::int *))
   (let* ([z::ScmStubType* (SCM_NEW ScmStubType)])
     (SCM_SET_CLASS z (& Scm_StubTypeClass))
     (set! (-> z name) (SCM_INTERN name))
     (set! (-> z c-of-type) c-of-type)
     (set! (-> z of-type) SCM_FALSE)
     (return (SCM_OBJ z))))

 (define-cise-stmt define-stub-type
   [(_ name fn)
    `(let* ([z (Scm_MakeStubType ,name ,fn)])
       (Scm_MakeBinding (Scm_GaucheModule)
                        (SCM_SYMBOL (-> (SCM_STUB_TYPE z) name)) z
                        SCM_BINDING_INLINABLE))])

 (define-cfn stub_fixnumP (obj) ::int :static
   (return (SCM_INTP obj)))

 ;; NB: Range check is also in ext/uvector/uvector.scm.  May be integrated.
 (define-cfn stub_s8P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) -128)
                (<= (SCM_INT_VALUE obj) 127))))
 (define-cfn stub_u8P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) 0)
                (<= (SCM_INT_VALUE obj) 255))))
 (define-cfn stub_s16P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) -32768)
                (<= (SCM_INT_VALUE obj) 32767))))
 (define-cfn stub_u16P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) 0)
                (<= (SCM_INT_VALUE obj) 65535))))
 (define-cfn stub_s32P (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (return (or (SCM_INTP obj)
                    (and (SCM_BIGNUMP obj)
                         (>= (Scm_NumCmp obj '#x-8000_0000) 0)
                         (<= (Scm_NumCmp obj '#x7fff_ffff) 0))))
        (return (and (SCM_INTP obj)
                     (>= (SCM_INT_VALUE obj) #x-8000_0000)
                     (<= (SCM_INT_VALUE obj) #x7fff_ffff)))))
 (define-cfn stub_u32P (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (return (or (and (SCM_INTP obj)
                         (>= (SCM_INT_VALUE obj) 0))
                    (and (SCM_BIGNUMP obj)
                         (>= (Scm_Sign obj) 0)
                         (<= (Scm_NumCmp obj '#xffff_ffff) 0))))
        (return (and (SCM_INTP obj)
                     (>= (SCM_INT_VALUE obj) 0)
                     (<= (SCM_INT_VALUE obj) #xffff_ffff)))))
 (define-cfn stub_s64P (obj) ::int :static
   (return (or (SCM_INTP obj)
               (and (SCM_BIGNUMP obj)
                    (>= (Scm_NumCmp obj '#x-8000_0000_0000_0000) 0)
                    (<= (Scm_NumCmp obj '#x-7fff_ffff_ffff_ffff) 0)))))
 (define-cfn stub_u64P (obj) ::int :static
   (return (or (and (SCM_INTP obj)
                    (>= (SCM_INT_VALUE obj) 0))
               (and (SCM_BIGNUMP obj)
                    (>= (Scm_Sign obj) 0)
                    (<= (Scm_NumCmp obj '#xffff_ffff_ffff_ffff) 0)))))

 (define-cfn stub_shortP (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) SHRT_MIN)
                (<= (SCM_INT_VALUE obj) SHRT_MAX))))
 (define-cfn stub_ushortP (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) 0)
                (<= (SCM_INT_VALUE obj) USHRT_MAX))))
 (define-cfn stub_intP (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (if (SCM_BIGNUMP obj)
          (let* ([oor::int FALSE]
                 [v::long (Scm_GetIntegerClamp obj SCM_CLAMP_BOTH (& oor))])
            (return (and (not oor)
                         (>= v INT_MIN)
                         (<= v INT_MAX))))
          (return (SCM_INTP obj)))
        (if (SCM_INTP obj)
          (let* ([v::long (SCM_INT_VALUE obj)])
            (return (and (>= v INT_MIN)
                         (<= v INT_MAX))))
          (return FALSE))))
 (define-cfn stub_uintP (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (if (SCM_BIGNUMP obj)
          (let* ([oor::int FALSE]
                 [v::u_long (Scm_GetIntegerUClamp obj SCM_CLAMP_BOTH (& oor))])
            (return (not oor)))
          (return (and (SCM_INTP obj) (>= (SCM_INT_VALUE obj) 0))))
        (return (and (SCM_INTP obj)
                     (>= (SCM_INT_VALUE obj) 0)
                     (<= (SCM_INT_VALUE obj) UINT_MAX)))))
 (define-cfn stub_longP (obj) ::int :static
   (if (SCM_BIGNUMP obj)
     (let* ([oor::int FALSE])
       (cast void (Scm_GetIntegerClamp obj SCM_CLAMP_BOTH (& oor)))
       (return (not oor)))
     (return (SCM_INTP obj))))
 (define-cfn stub_ulongP (obj) ::int :static
   (if (SCM_BIGNUMP obj)
     (let* ([oor::int FALSE])
       (cast void (Scm_GetIntegerUClamp obj SCM_CLAMP_BOTH (& oor)))
       (return (not oor)))
     (return (and (SCM_INTP obj) (>= (SCM_INT_VALUE obj) 0)))))

 ;; we don't range-check flonums
 (define-cfn stub_realP (obj) ::int :static
   (return (SCM_REALP obj)))
 (define-cfn stub_cstrP (obj) ::int :static
   (return (SCM_STRINGP obj)))

 (initcode
  (define-stub-type "<fixnum>"  stub_fixnumP)
  (define-stub-type "<short>"   stub_shortP)
  (define-stub-type "<ushort>"  stub_ushortP)
  (define-stub-type "<int>"     stub_intP)
  (define-stub-type "<uint>"    stub_uintP)
  (define-stub-type "<long>"    stub_longP)
  (define-stub-type "<ulong>"   stub_ulongP)
  (define-stub-type "<int8>"    stub_s8P)
  (define-stub-type "<uint8>"   stub_u8P)
  (define-stub-type "<int16>"   stub_s16P)
  (define-stub-type "<uint16>"  stub_u16P)
  (define-stub-type "<int32>"   stub_s32P)
  (define-stub-type "<uint32>"  stub_u32P)
  (define-stub-type "<int64>"   stub_s64P)
  (define-stub-type "<uint64>"  stub_u64P)
  (define-stub-type "<float>"   stub_realP)
  (define-stub-type "<double>"  stub_realP)
  (define-stub-type "<const-cstring>" stub_cstrP)
  ))

;;;
;;; Make exported symbol visible from outside
;;;

;; TRANSIENT: the inlinable flag is only necessary in 0.9.10 -> 0.9.11
;; transition, for define-class doesn't create inlinable binding in 0.9.10 but
;; it does in 0.9.11.
(let ((xfer (with-module gauche.internal %transfer-bindings)))
  (xfer (current-module)
        (find-module 'gauche)
        '(<type-constructor-meta>
          <descriptive-type>
          <^> </> <?> <Tuple> <List> <Vector>
          of-type?)
        '(inlinable))
  (xfer (current-module)
        (find-module 'gauche.internal)
        '(deconstruct-type
          wrap-with-proxy-type
          proxy-type-ref
          proxy-type-id)))
