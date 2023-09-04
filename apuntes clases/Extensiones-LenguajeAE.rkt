#lang play

#|
<Src> ::=   <number?>
          | {'+ <Src> <Src>}
          | {'- <Src> <Src>}
          | {'* <Src> ... <Src>}
          | {'- <Src>}

<Expr> ::=   (num <number?>)
           | (add <Expr> <Expr>)
           | (sub <Expr> <Expr>)
           | (mult <list>)
           | (neg <Expr>)

|#
(deftype Expr
  [num n]
  [add l r]
  [sub l r]
  [mult vals]
  [neg n]
  )

; interp :: Expr -> number?
; evalua una expresion aritmetica.

(define (interp expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(mult vals) (foldl * 1 (map interp vals))]
    [(neg n) (- (interp n))]
))

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)] 
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(cons '* vals) (mult (map parse vals))]
    [(list '- s1) (neg (parse s1))]
    )
  )

; run: Src -> Expr
; corre un programa
(define (run prog)
  (interp (parse prog))
  )

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{- 5 {+ 2 3}}) 0)
(test (run '{- 1}) -1)
(test (run '{- {+ 2 3}}) -5)
(test (run '{* 2 3 4}) 24)

#|

(* 3 2)
(* 1 2 3 4 5)


|#