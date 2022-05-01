#lang plait

;;1/6 cast/arrays - moved types out to give more common access to typed-class, typed-parse, and inherit specifically

(define-type Type
  (numT)
  (arrayT [array-type : Type]);;6: arrays
  (objT [class-name : Symbol]))

(module+ test
  (print-only-errors #t))

(define (parse-type [s : S-Exp]) : Type
  (cond
    [(s-exp-match? `num s)
     (numT)]
    [(s-exp-match? `SYMBOL s)
     (objT (s-exp->symbol s))]
    [(s-exp-match? `(< ANY >) s) ;;6: arrays
     (arrayT (parse-type (second (s-exp->list s))))]
    [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse-type `num)
        (numT))
  ;;6: arrays
  (test (parse-type `{< num >})
        (arrayT (numT)))
  (test (parse-type `{< {< num >} >})
        (arrayT (arrayT (numT)))) 
  (test (parse-type `Object)
        (objT 'Object))
  (test/exn (parse-type `{})
            "invalid input")
  )