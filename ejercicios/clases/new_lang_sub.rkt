#lang play

; mon 21 aug - creando un parser y un interprete

#|

Lenguaje AE -> Arithmetich Expression

1. Concrete syntax
2. Abstract syntax
3. parse
4. interp


<expr> ::=   (num <number?>)
           | (sub <expr> <expr>)

|#

(deftype Expr
  [num n]
  [sub l r]
  )

; interp : expr -> val
(define (interp expr)
  (match expr
    [(num n) n]
    [(sub l r) (- (interp l) (interp r))]
    ))

(test (interp (num 6)) 6)
(test (interp (sub (num 3) (num 4))) -1)



#|

<Src> ::= <number?> 
         | {'+ <Src> <Src>}

|#

; parse : src -> expr
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(list '- l r) (sub (parse l) (parse r))]
    )
  )

(test (parse 6) (num 6))
(test (parse '{- 3 4}) (sub (num 3) (num 4)))



; run: src -> val
(define (run prog)
  (interp (parse prog)))


(run '{- 3 {- 2 1}})













