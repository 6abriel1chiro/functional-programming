#lang play
#|
PROGRAMACIÓN FUNCIONAL
Examen - Sección 1.
Nombre: 

**Usando la metodología de declaración de funciones vista en clases responde los siguientes ejercicios**
No es necesario que resuelvas todos los ejercicios, resuelve los que puedas para llegar al máximo
de 5 puntos. Utiliza los tests de ejemplo y si agrega otros si lo consideras necesario. 

1. Escribe una función (freq lst num) que al recibir un numero y una lista devuelva la frecuencia
del número en la lista. (1pt):

(test (freq 1 '()) 0)
(test (freq 1 '(1 3 4 52 34 1 9 -3)) 2)


2. Define una función (equals-to k), que permita hacer lo siguiente (1pt):

(map (equals-to 2) '(1 2 4 2 6)) -> '(#f #t #f #t #f)

(test (map (equals-to 2) '(1 2 4 2 6))  '(#f #t #f #t #f))
(test (filter (equals-to 4) '(1 2 4 2 6)) '(4))
(test (filter (equals-to 1) '()) '())


3. Usando inducción estructural (escribe la fórmula de inducción y usa deftype para el árbol),
escriba alguna de las siguientes funciones:
  1. (sum-bt bt), que suma todos los elementos de un árbol binario (1pt)

(test (sum-bt (leaf 3)) 3)
(test (sum-bt (node 4 (leaf 3) (leaf 1))) 8)
(test (sum-bt (node 1 (node 1 (leaf 1) (leaf 1)) (leaf 1))) 5)

  2. (map-bt fun bt), que aplica una función a todos los elementos de un árbol (2pt)

(test (map-bt even? (leaf 3)) (leaf #f))
(test (map-bt odd? (node 4 (leaf 3) (leaf 1))) (node #f (leaf #t) (leaf #t)))
(test (map-bt number->string (node 1 (node 1 (leaf 1) (leaf 1)) (leaf 1))) (node "1" (node "1" (leaf "1") (leaf "1")) (leaf "1")))



4. Considerando el lenguaje AE visto en clases, extiendelo para que soporte multiplicación
y cambio de signo de un número, haz el cambio tanto en las funciones de parse e interp como
en las definiciones (2pt).


5. Un set es una estructura de datos en la que ningún elemento se repite, es decir, cada elemento
es unico, dada una lista lst, define la funcion (set? lst) que determina si una lista cumple con
la condicion de ser un set. Considera que una lista vacia no es un set (2pt).

(test (set? '()) #f)
(test (set? '(1)) #t)
(test (set? '(1 2 3 4)) #t)
(test (set? '(1 2 2)) #f)
|#





; 4. AE
#|
<Src> ::=   <number?>
          | {'+ <Src> <Src>}
          | {'- <Src> <Src>}

<Expr> ::=   (num <number?>)
           | (add <Expr> <Expr>)
           | (sub <Expr> <Expr>)
|#
(deftype Expr
  [num n]
  [add l r]
  [sub l r]
  )

; interp :: Expr -> number?
; evalua una expresion aritmetica.

(define (interp expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
))

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)] 
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
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
