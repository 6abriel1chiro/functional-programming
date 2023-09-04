#lang play
#|

PROGRAMACIÓN FUNCIONAL
Examen - Sección 1.

**Usando la metodología de declaración de funciones vista en clases responde los siguientes ejercicios**
No es necesario que resuelvas todos los ejercicios, resuelve los que puedas para llegar al máximo
de 5 puntos. Utiliza los tests de ejemplo y si agrega otros si lo consideras necesario. 

1. Escribe una funcion (freq lst num) que al recibir un numero y
una lista devuelva la frecuencia del numero en la lista. Escribe
esta funcion de dos maneras:
  1. Manual (1pt)
  2. Usa algun camino mas corto. (1pt)

(1pt)
2. Define una funcion (equals-to k), que permita hacer lo siguiente:

(map (equals-to 2) '(1 2 4 2 6)) -> '(#f #t #f #t #f)


3. Usando induccion estructural, escriba alguna de las siguientes funciones:
  1. (sum-bt bt), que suma todos los elementos de un arbol binario (1pt)
  2. (map-bt fun bt), que aplica una funcion a todos los elementos de un arbol (2pt)

(2pt)
4. Considerando el lenguaje AE visto en clases, extiendelo para
que soporte multiplicacion y cambio de signo de un numero

(2pt)
5. Un set es una estructura de datos en la que ningún elemento
se repite, es decir, cada elemento es unico, dada una lista lst,
define la funcion (set? lst) que determina si una lista cumple con
la condicion de ser un set. Considera que una lista vacia no es un set. 

|#

; 1. freq

; freq: num lst -> num
; Cuenta la frecuencia de un numero en una lista

; def manual --> (pattern matching), tambien puede hacerse con if
(define (freq num lst)
  (match lst
    [(list) 0]
    [(cons head tail) (if (eq? head num)
                          (+ 1 (freq num tail))
                          (freq num tail))
                          ]
    )
  )

(test (freq 1 '()) 0)
(test (freq 1 '(1 3 4 52 34 1 9 -3)) 2)


; def otra logica

(define (freq-n num lst)
  (length (filter (λ (x) (eq? num x)) lst))
  )

(test (freq-n 1 '()) 0)
(test (freq-n 1 '(1 3 4 52 34 1 9 -3)) 2)


; 2. equals-to

; equals-to: num -> λ
; Devuelve una funcion de comparacion al numero enviado
(define (equals-to num)
  (λ (x) (eq? x num))
  )

(test (map (equals-to 2) '(1 2 4 2 6))  '(#f #t #f #t #f))
(test (filter (equals-to 4) '(1 2 4 2 6)) '(4))
(test (filter (equals-to 1) '()) '())


; 3. sum-bt
#|
<bintree> ::=    (leaf val)
               | (node val <bintree> <bintree>)


(define (fun bt)
   (match bt
     [(leaf v) ...]
     [(node v l r) ... v ...(fun l) ... (fun r)]
   )
)
|#

(deftype Bintree
  [leaf val]
  [node val left right]
  )

; sum-bt: bt -> num
; Obtiene la suma de los elementos de un arbol

(define (sum-bt bt)
   (match bt
     [(leaf v) v]
     [(node v l r) (+ v (sum-bt l) (sum-bt r))]
   )
)

(test (sum-bt (leaf 3)) 3)
(test (sum-bt (node 4 (leaf 3) (leaf 1))) 8)
(test (sum-bt (node 1 (node 1 (leaf 1) (leaf 1)) (leaf 1))) 5)


; map-bt: fun bt -> (fun bt)
; mapea una funcion a un arbol

(define (map-bt fun bt)
  (match bt
    [(leaf v) (leaf (fun v))]
    [(node v l r) (node (fun v) (map-bt fun l) (map-bt fun r))]
    )
  )
(test (map-bt even? (leaf 3)) (leaf #f))
(test (map-bt odd? (node 4 (leaf 3) (leaf 1))) (node #f (leaf #t) (leaf #t)))
(test (map-bt number->string (node 1 (node 1 (leaf 1) (leaf 1)) (leaf 1))) (node "1" (node "1" (leaf "1") (leaf "1")) (leaf "1")))



; 4. mult + neg
#|
<Src> ::=   <number?>
          | {'+ <Src> <Src>}
          | {'- <Src> <Src>}
          | {'* <Src> <Src>}
          | {'- <Src>}

<Expr> ::=   (num <number?>)
           | (add <Expr> <Expr>)
           | (sub <Expr> <Expr>)
           | (mult <Expr> <Expr>)
           | (neg <Expr>)

|#
(deftype Expr
  [num n]
  [add l r]
  [sub l r]
  [mult l r]
  [neg n]
  )


; interp :: Expr -> number?
; evalua una expresion aritmetica.

(define (interp expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(mult l r) (* (interp l) (interp r))]
    [(neg n) (- (interp n))]
))

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)] 
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(list '* s1 s2) (mult (parse s1) (parse s2))]
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

; 5. set?
; set?: list -> boolean
; define si una lista representa un set
(define (set? lst)
  (match lst
    [(list) #f]
    [(cons head (list)) #t]
    [(cons head tail) (let ([freq-elem (freq head lst)])
                        (if (eq? freq-elem 1)
                            (and #t (set? tail))
                            #f)
                        )]
    )
  )

(test (set? '()) #f)
(test (set? '(1)) #t)
(test (set? '(1 2 3 4)) #t)
(test (set? '(1 2 2)) #f)

