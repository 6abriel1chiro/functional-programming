#lang play
#|

Lenguaje AE -> Arithmetic Expression

1. Concrete syntax
2. Abstract syntax
3. parse
4. interp

(num 6)
(add (num 3) (num 4))
(add (num 3) (add (num 4) (num 1)))

<expr> ::=   (num <number?>)
           | (bool <boolean?>)
           | (add <expr> <expr>)

|#

(deftype Expr
  [num n]
  [bool b]
  [add l r]
  )

; interp : expr -> val
(define (interp expr)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(add l r) (+ (interp l) (interp r))]
    ))

(test (interp (num 6)) 6)
(test (interp (add (num 3) (num 4))) 7)



#|

<Src> ::= <number?> | <boolean?> 
         | {'+ <Src> <Src>}

|#

; parse : src -> expr
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(list '+ l r) (add (parse l) (parse r))]
    )
  )

(test (parse 6) (num 6))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))

(deftype Type
  (Num)
  (Bool))


;typeof: expr -> type/error
(define (typeof expr)
  (match expr
    [(num n) (Num)]
    [(bool b) (Bool)]
    [(add l r) (let ([tl (typeof l)]
                     [tr (typeof r)])
                 (if (and (Num? tl) (Num? tr))
                     (Num)
                     (error "type error")))
     ]
    )
  )



; run: src -> val
(define (run prog)
  (let* ([expr (parse prog)]
         [t (typeof expr)])
         (interp expr)
         )
    )
 


(run '{+ 3 {+ 2 1}})
(run #t)
(run '{+ 1 #t})





