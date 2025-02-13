#lang plait

(require "class.rkt"
         "inherit.rkt"
         "types.rkt")

(define-type ClassT
  (classT [super-name : Symbol]
          [fields : (Listof (Symbol * Type))]
          [methods : (Listof (Symbol * MethodT))]))

(define-type MethodT
  (methodT [arg-type : Type]
           [result-type : Type]
           [body-expr : ExpI]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'Object)
      empty        
      (type-case ClassT (find t-classes class-name)
        [(classT super-name fields methods)
         (append 
          (get-all-field-types super-name t-classes)
          (map snd fields))])))

;; ----------------------------------------

(define (make-find-in-tree class-items)
  (lambda (name class-name t-classes)
    (local [(define t-class (find t-classes class-name))
            (define items (class-items t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'Object)
          (find items name)
          (try (find items name)
               (lambda ()
                 ((make-find-in-tree class-items)
                  name 
                  super-name
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree classT-fields))

(define find-method-in-tree
  (make-find-in-tree classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) #t]
    [(equal? name1 'Object) #f]
    [else
     (type-case ClassT (find t-classes name1)
       [(classT super-name fields methods)
        (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [(objT name1)
     (type-case Type t2 
       [(objT name2)
        (if (equal? 'null name2) ;;8: null - null is a subtype and supertype of every object
            #t
            (if (equal? 'null name1) ;;8: null - null is a subtype and supertype of every object
                #t
                (is-subclass? name1 name2 t-classes)))]
       [else #f])]
    [(arrayT a-t1) ;;6: arrays
     (type-case Type t2 
       [(arrayT a-t2) ;;not option 7 so not subtype if their types are subtypes
        (equal? a-t1 a-t2)]
       [else #f])]
    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (values 'A (classT 'Object empty empty)))
  (define b-t-class (values 'B (classT 'A empty empty)))

  (test (is-subclass? 'Object 'Object empty)
        #t)
  (test (is-subclass? 'A 'B (list a-t-class b-t-class))
        #f)
  (test (is-subclass? 'B 'A (list a-t-class b-t-class))
        #t)

  (test (is-subtype? (numT) (numT) empty)
        #t)
  (test (is-subtype? (numT) (objT 'Object) empty)
        #f)

  ;;6: array - test - is-subtype?
  (test (is-subtype? (arrayT (objT 'null)) (objT 'null) empty)
        #f)
  (test (is-subtype? (objT 'null) (arrayT (objT 'null)) empty)
        #f)
  (test (is-subtype? (objT 'Object) (arrayT (objT 'Object)) empty)
        #f)
  (test (is-subtype? (arrayT (objT 'Object)) (objT 'Object) empty)
        #f)
  (test (is-subtype? (arrayT (numT)) (numT) empty)
        #f)
  (test (is-subtype? (numT) (arrayT (numT))  empty)
        #f)
  (test (is-subtype? (arrayT (numT)) (arrayT (numT))  empty)
        #t)
  (test (is-subtype? (arrayT (arrayT (numT))) (arrayT (numT))  empty)
        #f)
  (test (is-subtype? (objT 'Object) (arrayT (objT 'Object))  empty)
        #f)
  (test (is-subtype? (arrayT (objT 'Object)) (arrayT (objT 'Object))  empty)
        #t)
  (test (is-subtype? (arrayT (arrayT (objT 'Object))) (arrayT (objT 'Object))  empty)
        #f)
  (test (is-subtype? (objT 'Object) (arrayT (objT 'Object))  empty)
        #f)
  (test (is-subtype? (arrayT (objT 'Object)) (arrayT (objT 'Object))  empty)
        #t)
  (test (is-subtype? (arrayT (arrayT (objT 'Object))) (arrayT (objT 'Object))  empty)
        #f)
  (test (is-subtype? (arrayT (arrayT (objT 'Object))) (arrayT (arrayT (objT 'Object)))  empty)
        #t)
  (test (is-subtype? (arrayT (objT 'A)) (arrayT (objT 'Object))  (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (arrayT (objT 'Object)) (arrayT (objT 'A))  (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (arrayT (objT 'B)) (arrayT (objT 'Object))  (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (arrayT (objT 'Object)) (arrayT (objT 'B))  (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (arrayT (objT 'B)) (arrayT (objT 'A))  (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (arrayT (objT 'A)) (arrayT (objT 'B))  (list a-t-class b-t-class))
        #f)
  
  
  
  ;;8: null - test - is-subtype?
  (test (is-subtype? (objT 'null) (objT 'Object) empty)
        #t)
  (test (is-subtype? (objT 'Object) (objT 'null) empty)
        #t)
  (test (is-subtype? (numT) (objT 'null) empty)
        #f)
  (test (is-subtype? (objT 'null) (numT) empty)
        #f)

  (test (is-subtype? (objT 'Object) (numT) empty)
        #f)
  (test (is-subtype? (objT 'A) (objT 'B) (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (objT 'B) (objT 'A) (list a-t-class b-t-class))
        #t))

;; ----------------------------------------

(define typecheck-expr : (ExpI (Listof (Symbol * ClassT)) Type Type -> Type)
  (lambda (expr t-classes this-type arg-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes this-type arg-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [(numT)
                 (type-case Type (recur r)
                   [(numT) (numT)]
                   [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExpI expr
        [(numI n) (numT)]
        [(plusI l r) (typecheck-nums l r)]
        [(multI l r) (typecheck-nums l r)]
        [(argI) arg-type]
        
        ;;8: null
        [(nullI) (objT 'null)]

        ;;1: cast
        [(castI s e) (local [(define e-t (recur e))
                             (define s-t (parse-type (symbol->s-exp s)))]                                          
                       ;;ensure cast can succeed
                       (if (or (is-subtype? s-t e-t t-classes)
                               (is-subtype? e-t s-t t-classes))
                           s-t ;;succeeds
                           (type-error e (to-string s-t))))];;fails

        ;;6: arrays
        ;;ensure l is numT
        ;;if t doesnt match the type of v, then error
        ;;otherwise return array-t
        [(newarrayI t l v) (if (equal? (numT) (recur l))
                               (local [(define v-t (recur v))]
                                 (if (is-subtype? v-t t t-classes)
                                     (arrayT t)
                                     (type-error t (to-string t))))
                               (type-error l "arrayT"))]
        ;;ensure i is numT
        ;;always the type of the array so long as it is an array
        [(arrayrefI a i) (if (equal? (numT) (recur i))
                             (type-case Type (recur a)
                               [(arrayT t)(arrayT t)]
                               [else (type-error a "arrayT")])
                             (type-error i "arrayT"))]
        ;;ensure i is numT
        ;;determine type of array by recurring, if it is not an array then error
        ;;get the type of the type of the array by type case, if it doesnt match the type of v, then error
        ;;otherwise return numT
        [(arraysetI a i v) (if (equal? (numT) (recur i))
                               (local [(define a-t (recur a))
                                       (define v-t (recur v))]
                                 (type-case Type (recur a)
                                   [(arrayT t)
                                    (if (is-subtype? v-t t t-classes)
                                        (numT)
                                        (type-error v (to-string a-t)))]
                                   [else (type-error a "arrayT")]))
                               (type-error i "arrayT"))]

        ;;begin - always the type of s
        [(beginI f s)(recur s)]
                           
        
        ;;2: if0
        [(if0I c t e)
         (type-case Type (recur c)
           [(numT) (let ([t1 (recur t)]) (let ([t2 (recur e)])
                                       (find-lub t1 t2 e t-classes)))]
           [else (type-error c "num")])]
        
        [(thisI) this-type]
        [(newI class-name exprs)
         (local [(define arg-types (map recur exprs))
                 (define field-types
                   (get-all-field-types class-name t-classes))]
           (if (and (= (length arg-types) (length field-types))
                    (foldl (lambda (b r) (and r b))
                           #t
                           (map2 (lambda (t1 t2) 
                                   (is-subtype? t1 t2 t-classes))
                                 arg-types
                                 field-types)))
               (objT class-name)
               (type-error expr "field type mismatch")))]
        [(getI obj-expr field-name)
         (type-case Type (recur obj-expr)
           [(objT class-name)
            ;;8: null - error if trying to access fields of null
            (if (equal? class-name 'null)
                (type-error expr "object is null and has no definition")
                (find-field-in-tree field-name
                                    class-name
                                    t-classes))]
           [else (type-error obj-expr "object")])]
        [(sendI obj-expr method-name arg-expr)
         (local [(define obj-type (recur obj-expr))
                 (define arg-type (recur arg-expr))]
           (type-case Type obj-type
             [(objT class-name)
              ;;8: null - error if trying to send method to null
              (if (equal? class-name 'null)
                (type-error expr "object is null and has no definition")
                (typecheck-send class-name method-name
                                arg-expr arg-type
                                t-classes))]
             [else
              (type-error obj-expr "object")]))]
        [(superI method-name arg-expr)
         (local [(define arg-type (recur arg-expr))
                 (define this-class
                   (find t-classes (objT-class-name this-type)))]
           (typecheck-send (classT-super-name this-class)
                           method-name
                           arg-expr arg-type
                           t-classes))]))))

;;2: if0 - helper functions for finding LUB type
(define (find-lub [t1 : Type][t2 : Type][e2 : ExpI][t-classes : (Listof (Symbol * ClassT))]) : Type
  (type-case Type t1
    [(objT c1)
     (type-case Type t2 
       [(objT c2)
        (find-lub-recur c1 c2 c1 c2 t-classes)]
       [else (type-error e2 (to-string t1))])]
    [else (if (equal? t1 t2)
              t1
              (type-error e2 (to-string t1)))]))  
(define (find-lub-recur [c1 : Symbol][c2 : Symbol][super-c1 : Symbol][super-c2 : Symbol][t-classes : (Listof (Symbol * ClassT))]) : Type 
  (if (is-subclass? c1 super-c2 t-classes)
      (objT super-c2)
      (if (is-subclass? c2 super-c1 t-classes)
          (objT super-c1)
          (type-case ClassT (find t-classes super-c1)
            [(classT super-duper-c1 fields1 methods1)
             (type-case ClassT (find t-classes super-c2)
               [(classT super-duper-c2 fields2 methods2)             
                (find-lub-recur c1 c2 super-duper-c1 super-duper-c2 t-classes)])]))))


(define (typecheck-send [class-name : Symbol]
                        [method-name : Symbol]
                        [arg-expr : ExpI]
                        [arg-type : Type]
                        [t-classes : (Listof (Symbol * ClassT))])
  (type-case MethodT (find-method-in-tree
                      method-name
                      class-name
                      t-classes)
    [(methodT arg-type-m result-type body-expr)
     (if (is-subtype? arg-type arg-type-m t-classes)
         result-type
         (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (Listof (Symbol * ClassT))]) : ()
  (type-case MethodT method
    [(methodT arg-type result-type body-expr)
     (if (is-subtype? (typecheck-expr body-expr t-classes
                                      this-type arg-type)
                      result-type
                      t-classes)
         (values)
         (type-error body-expr (to-string result-type)))]))

(define (check-override [method-name : Symbol]
                        [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (Listof (Symbol * ClassT))])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree method-name
                                  super-name
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string method-name))))))

(define (typecheck-class [class-name : Symbol]
                         [t-class : ClassT]
                         [t-classes : (Listof (Symbol * ClassT))])
  (type-case ClassT t-class
    [(classT super-name fields methods)
     (map (lambda (m)
            (begin
              (typecheck-method (snd m) (objT class-name) t-classes)
              (check-override (fst m) (snd m) t-class t-classes)))
          methods)]))

(define (typecheck [a : ExpI]
                   [t-classes : (Listof (Symbol * ClassT))]) : Type
  (begin
    (map (lambda (tc)
           (typecheck-class (fst tc) (snd tc) t-classes))
         t-classes)
    (typecheck-expr a t-classes (objT 'Object) (numT))))

;; ----------------------------------------

(module+ test
  (define posn-t-class
    (values 'Posn
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'addDist
                                  (methodT (objT 'Posn) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))))))

  (define posn3D-t-class 
    (values 'Posn3D
            (classT 'Posn
                    (list (values 'z (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT)
                                           (plusI (getI (thisI) 'z) 
                                                  (superI 'mdist (argI)))))))))

  (define square-t-class 
    (values 'Square
            (classT 'Object
                    (list (values 'topleft (objT 'Posn)))
                    (list))))

  (define square-two-t-class 
    (values 'Square-two
            (classT 'Square
                    (list)
                    (list))))

  (define nullified-t-class ;;8: null
    (values 'Nullified
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (objT 'Posn)))
                    (list (values 'get-x
                                  (methodT (objT 'Posn3D) (numT)
                                           (getI (thisI) 'x)))
                          (values 'get-y
                                  (methodT (objT 'Posn3D) (objT 'Posn)
                                           (getI (thisI) 'y)))
                          (values 'get-x-wrap
                                  (methodT (objT 'Nullified) (numT)
                                           (sendI (argI) 'get-x (nullI))))))))
  
                                  

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class square-two-t-class)))

  (define (typecheck-nullified a)
    (typecheck a
               (list nullified-t-class posn-t-class posn3D-t-class)))
  
  (define new-posn27 (newI 'Posn (list (numI 2) (numI 7))))
  (define new-posn531 (newI 'Posn3D (list (numI 5) (numI 3) (numI 1))))

  (test (typecheck-posn (sendI new-posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI new-posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI new-posn531 'addDist new-posn27))
        (numT))  
  (test (typecheck-posn (sendI new-posn27 'addDist new-posn531))
        (numT))

  (test (typecheck-posn (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1))))))
        (objT 'Square))
  (test (typecheck-posn (newI 'Square (list (newI 'Posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'Square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  ;;2: if0 - test - typecheck
  (test (typecheck-posn (if0I (numI 1) (newI 'Object empty) (newI 'Posn (list (numI 0) (numI 0)))))
        (objT 'Object))
  (test (typecheck-posn (if0I (numI 1) (newI 'Square-two (list (newI 'Posn (list (numI 0) (numI 1))))) (newI 'Posn3D (list (numI 0) (numI 0) (numI 0)))))
        (objT 'Object))
  (test (typecheck-posn (if0I (numI 1) (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1))))) (newI 'Posn (list (numI 0) (numI 0)))))
        (objT 'Object))
  (test (typecheck-posn (if0I (numI 1) (numI 2) (numI -3)))
        (numT))
  (test (typecheck-posn (if0I (numI 1) (newI 'Posn3D (list (numI 0) (numI 0) (numI 0))) (newI 'Posn (list (numI 0) (numI 0)))))
        (objT 'Posn))
  (test (typecheck-posn (if0I (numI 1) (newI 'Posn (list (numI 0) (numI 0))) (newI 'Posn3D (list (numI 0) (numI 0) (numI 0)))))
        (objT 'Posn))
  (test (typecheck-posn (if0I (numI 1) (newI 'Object empty) (newI 'Posn3D (list (numI 0) (numI 0) (numI 0)))))
        (objT 'Object))
  (test (typecheck-posn (if0I (numI 1) (newI 'Posn3D (list (numI 0) (numI 0) (numI 0))) (newI 'Posn3D (list (numI 0) (numI 0) (numI 0)))))
        (objT 'Posn3D))
  (test/exn (typecheck (if0I (newI 'Object empty) (numI 2) (numI -3)) empty)
            "no type")
  (test/exn (typecheck (if0I (numI 99) (newI 'Object empty) (numI -3)) empty)
            "no type")
  (test/exn (typecheck (if0I (numI 99) (numI -3) (newI 'Object empty)) empty)
            "no type")

  ;;8: null - test - typecheck
  (test (typecheck (nullI) empty)
        (objT 'null))
  (test (typecheck-nullified (sendI (newI 'Nullified (list (numI 10) new-posn27)) 'get-x (nullI)))
        (numT))
  (test (typecheck-nullified (sendI (newI 'Nullified (list (numI 10) (nullI))) 'get-x (nullI)))
        (numT))
  (test/exn (typecheck-nullified (sendI (newI 'Nullified (list (nullI) (nullI))) 'get-x (nullI)))
            "no type")
  (test (typecheck-nullified (sendI (newI 'Nullified (list (numI 10) (nullI))) 'get-x-wrap (newI 'Nullified (list (numI 10) (nullI)))))
        (numT))
  ;;call get or send directly on null returns error as there can be no class definition of null to check
  (test/exn (typecheck (getI (nullI) 'x) empty)
            "object is null and has no definition")
  (test/exn (typecheck (sendI (nullI) 'fn (numI 0)) empty)
            "object is null and has no definition")
  ;;call get or send on object given a null value will only error at runtime when interping
  (test (typecheck-posn (sendI new-posn27 'addDist (nullI)))
        (numT))
  (test (typecheck-nullified (sendI (newI 'Nullified (list (numI 10) (nullI))) 'get-x-wrap (nullI)))
        (numT))
  ;;since null is defined to be a supertype of posn this is fine, also it is legal get a null object just not to get "from" a null object
  (test (typecheck-nullified (sendI (newI 'Nullified (list (numI 10) (nullI))) 'get-y (nullI)))
        (objT 'Posn))

  ;;1: cast - test - typecheck
  (test/exn (typecheck (castI 'num (nullI)) empty)
            "no type")
  (test (typecheck (castI 'Object (nullI)) empty)
        (objT 'Object))
  (test (typecheck-posn (castI 'Object new-posn27))
        (objT 'Object))
  (test (typecheck-posn (castI 'Posn new-posn27))
        (objT 'Posn))
  (test (typecheck-posn (castI 'Posn3D new-posn27))
        (objT 'Posn3D)) 
  (test/exn (typecheck-nullified (castI 'Nullified new-posn27))
            "no type")

  ;;6: arrays/begin - test - typecheck
  (test (typecheck (newarrayI (numT) (numI 10) (numI 2)) empty)
        (arrayT(numT)))
  (test/exn (typecheck (newarrayI (arrayT (numT)) (numI 10) (numI 2)) empty)
            "no type")
  (test/exn (typecheck (newarrayI (numT) (newI 'Object empty) (numI 2)) empty)
            "no type")
  (test (typecheck (arrayrefI (newarrayI (numT) (numI 10) (numI 2)) (numI 2)) empty)
         (arrayT(numT)))
  (test/exn (typecheck (arrayrefI (newarrayI (numT) (numI 10) (numI 2)) (newI 'Object empty)) empty)
            "no type")
  (test/exn (typecheck (arrayrefI (newI 'Object empty) (numI 1)) empty)
            "no type")
  (test (typecheck (arraysetI (newarrayI (numT) (numI 10) (numI 2)) (numI 2) (numI 3)) empty)
         (numT))
  (test/exn (typecheck (arraysetI (newarrayI (numT) (numI 10) (numI 2)) (newI 'Object empty) (numI 3)) empty)
            "no type")
  (test/exn (typecheck (arraysetI (newarrayI (numT) (numI 10) (numI 2)) (numI 1) (newI 'Object empty)) empty)
            "no type")
  (test/exn (typecheck (arraysetI (newI 'Object empty) (numI 1) (numI 3)) empty)
            "no type")
  (test (typecheck (beginI (numI 1) (numI 2)) empty)
                  (numT))
  
  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI new-posn27 'mdist new-posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'Object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class
                             (values 'Other
                                     (classT 'Posn
                                             (list)
                                             (list (values 'mdist
                                                           (methodT (objT 'Object) (numT)
                                                                    (numI 10))))))))
            "bad override")
  (test/exn (typecheck-method (methodT (numT) (objT 'Object) (numI 0)) (objT 'Object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (values 'Cube
                                     (classT 'Square
                                             empty
                                             (list
                                              (values 'm
                                                      (methodT (numT) (numT)
                                                               ;; No such method in superclass:
                                                               (superI 'm (numI 0)))))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [(classT super-name fields methods)
       (classI
        super-name
        (map fst fields)
        (map (lambda (m)
               (values (fst m)
                       (type-case MethodT (snd m)
                         [(methodT arg-type result-type body-expr)
                          body-expr])))
             methods))])))
  
(define interp-t : (ExpI (Listof (Symbol * ClassT)) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map (lambda (c)
                     (values (fst c) (strip-types (snd c))))
                   t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI new-posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI new-posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI new-posn531 'addDist new-posn27))
        (numV 18))
  (test (interp-t-posn (sendI new-posn27 'addDist new-posn531))
        (numV 18)))