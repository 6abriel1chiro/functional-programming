#lang play
#|
Mini repaso:

Funciones como valores
1. Parametros (map/fold/reduce)
2. λ -> funciones anonimas
3. Retorno de funciones

--> Fundamental del paradigma funcional.

* Recursividad
Correspondencia Curry Howard
Induccion ~= Recursion

--> Induccion Estructural

<bintree> ::= (leaf val)
              | (node val <bintree> <bintree>)

(define (f bintree)
    (match bintree
       [(leaf v) ...]
       [(node v l r) ... (f l) ... (f r) ...]

))

--> Utilizar lo basico -> entender un lenguaje (funcionamiento / semantica)
-> Sintaxis Concreta
<Src> ::=   <number?>
          | {'+ <Src> <Src>}
          | {'- <Src> <Src>}

5
{+ 1 3}
{+ 1 {+ 2 3}}


-> Sintaxis abstracta
<Expr> ::=   (num <number?>)
           | (add <Expr> <Expr>)
           | (sub <Expr> <Expr>)

|#

(deftype Expr
  [num n]
  [add l r]
  [sub l r])


; calc :: Expr -> number?
; evalua una expresion aritmetica.

(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
))

(test (calc (num 3)) 3)
(test (calc (add (num 3) (num 1))) 4)

; {} ()

; 1ra fase -> Lexer/Tokenizer
; + 1 4 -> t+ t1 t4
; 2da fase -> Parse

; parse: Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)] ; si pasa el filtro, me interesa
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    )
  )


(define (run prog)
  (calc (parse prog))
  )

(run '{+ 3 4})



