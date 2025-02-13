#lang plait
(require "class.rkt"
         "inherit.rkt"
         "types.rkt")

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (parse-class [s : S-Exp]) : (Symbol * ClassI)
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     ;;8: null - disallow null as a class name
     (values (let [(name (s-exp->symbol (second (s-exp->list s))))]
               (if (equal? 'null name)
                    (error 'parse-t-class "invalid input: null is not a valid class name")
                    name))
             (classI
              (s-exp->symbol (fourth (s-exp->list s)))
              (map parse-field
                   (s-exp->list (fourth (rest (s-exp->list s)))))
              (map parse-method 
                   (rest (rest (rest (rest (rest (s-exp->list s)))))))))]
   [else (error 'parse-class "invalid input")]))

(define (parse-field [s : S-Exp]) : Symbol
  (cond
   [(s-exp-match? `SYMBOL s)
    (s-exp->symbol s)]
   [else (error 'parse-field "invalid input")]))

(define (parse-method [s : S-Exp]) : (Symbol * ExpI)
  (cond
   [(s-exp-match? `[SYMBOL {arg} ANY] s)
    (values (s-exp->symbol (first (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [else (error 'parse-method "invalid input")]))

(define (parse [s : S-Exp]) : ExpI
  (cond
   [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
   [(s-exp-match? `arg s) (argI)]
   [(s-exp-match? `this s) (thisI)]
   [(s-exp-match? `{+ ANY ANY} s)
    (plusI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? `{* ANY ANY} s)
    (multI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   ;;2: If0
   [(s-exp-match? `{if0 ANY ANY ANY} s)
    (if0I (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]

   ;;6: arrays & begin -> with separate types - new array directly pases-type from the first ANY
   [(s-exp-match? `{newarray ANY ANY ANY} s)
    (newarrayI (parse-type (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? `{arrayref ANY ANY} s)
    (arrayrefI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? `{arrayset ANY ANY ANY} s)
    (arraysetI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   ;;6: arrays & begin
    [(s-exp-match? `{begin ANY ANY} s)
    (beginI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]

   ;;8: null
   [(s-exp-match? `null s) (nullI)]

   ;;1: cast
   [(s-exp-match? `{cast SYMBOL ANY} s)
    (castI (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   
   [(s-exp-match? `{new SYMBOL ANY ...} s)
    (newI (s-exp->symbol (second (s-exp->list s)))
          (map parse (rest (rest (s-exp->list s)))))]
   [(s-exp-match? `{get ANY SYMBOL} s)
    (getI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s))))]
   [(s-exp-match? `{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? `{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  
  ;;2: If0 - test - parse
  (test (parse `{if0 1 -1 2})
        (if0I (numI 1) (numI -1) (numI 2)))

  ;;8: null - test - parse
  (test (parse `null)
        (nullI))
  ;;8: null - test - parse-class of null named provides invalid input
  (test/exn (parse-class `{class null extends Object {}})
            "invalid input")
  
  ;;1: cast - test - parse
  (test (parse `{cast Object {new Object 1}})
        (castI 'Object (newI 'Object (list (numI 1)))))

  ;;6: array
  (test (parse `{newarray num 5 1})
        (newarrayI (numT) (numI 5) (numI 1)))
  (test (parse `{arrayref {newarray num 5 1} 0})
        (arrayrefI (newarrayI (numT) (numI 5) (numI 1))(numI 0)))
  (test (parse `{arrayset {newarray num 5 1} 0 -9})
        (arraysetI (newarrayI (numT) (numI 5) (numI 1))(numI 0) (numI -9)))
  (test (parse `{newarray {< num >} 5 {newarray num 5 1}})
        (newarrayI (arrayT (numT)) (numI 5) (newarrayI (numT) (numI 5) (numI 1))))
  (test (parse `{begin 5 2})
        (beginI (numI 5) (numI 2)))
  
  (test (parse `{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse `{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse `{new Posn 1 2})
        (newI 'Posn (list (numI 1) (numI 2))))
  (test (parse `{get 1 x})
        (getI (numI 1) 'x))
  (test (parse `{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse `{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field `{x 1})
            "invalid input")

  (test (parse-method `[m {arg} this])
        (values 'm (thisI)))
  (test/exn (parse-method `[m {arg} 1 2])
            "invalid input")
  
  (test (parse-class `{class Posn3D extends Posn
                        {x y z}
                        [m1 {arg} arg]
                        [m2 {arg} this]})
        (values 'Posn3D
                (classI 'Posn
                        (list 'x 'y 'z)
                        (list (values 'm1 (argI))
                              (values 'm2 (thisI))))))
  (test/exn (parse-class `{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-prog [classes : (Listof S-Exp)] [a : S-Exp]) : S-Exp
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [(numV n) (number->s-exp n)]

      ;;8: null - case for null
      [(nullV) `null] 

      ;;6: arrays - case for array
      [(arrayV vs) `array]
      
      [(objV class-name field-vals) `object])))

(module+ test
  (test (interp-prog
         (list
          `{class Empty extends Object
             {}})
         `{new Empty})
        `object)

  ;;8: null - interp prog
  (test (interp-prog
         (list
          `{class Empty extends Object
             {}})
         `null)
        `null)

  ;;6: array - interp prog
  (test (interp-prog
         (list
          `{class Empty extends Object
             {}})
         `{newarray num 5 1})
        `array)

  ;;2: if0 - test - interp-t-prog
  (test (interp-prog
         (list
          `{class Empty extends Object
             {}})
         `{if0 1 -10 10})
        `10)

  ;;1: cast - test - interp-prog
  (test (interp-prog 
        (list
         `{class Posn extends Object
            {x y}
            [mdist {arg} {+ {get this x} {get this y}}]
            [addDist {arg} {+ {send arg mdist 0}
                              {send this mdist 0}}]}
         
         `{class Posn3D extends Posn
            {z}
            [mdist {arg} {+ {get this z} 
                            {super mdist arg}}]})
        
        `{send {cast Posn {new Posn3D 2 5 6}} addDist {new Posn 1 2}})
       `16)

 (test (interp-prog 
        (list
         `{class Posn extends Object
            {x y}
            [mdist {arg} {+ {get this x} {get this y}}]
            [addDist {arg} {+ {send arg mdist 0}
                              {send this mdist 0}}]}
         
         `{class Posn3D extends Posn
            {z}
            [mdist {arg} {+ {get this z} 
                            {super mdist arg}}]})
        
        `{send {new Posn3D 5 3 1} addDist {new Posn 2 7}})
       `18))
