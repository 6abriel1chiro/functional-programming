#lang play


; practica 1.2
; 1. implementar map

#|

Contract: map recibe una funcion y una lista
          devuelve una nueva lista con los valores de aplicar la funcion a cada elemento de la lista original
In: -fun: fncion a aplicar 
    -lst: lista a la que se aplica la funcion
Out: la nueva lista con los valores resultantes
|#

(print "EJERCICIO 1")
(newline)


(define (map fun lst)
  (if (empty? lst)
      '()
      (cons(fun (first lst)) (map fun (rest lst))
      )
  )
  )

(test (map add1 '(1 2 3 4)) '(2 3 4 5))
(test (map (λ (n)(* n 3)) '(2 4 6 8)) '(6 12 18 24))
(test (map (λ (n)(* n 3)) '()) '())



; ejer 2 prac 1.2

;Define una función recursiva que emule el comportamiento de foldr: (define (my-foldr fun initVal lst))

#|
Contract: my-foldr recibe una funcion, un valor inicial y una lista
          aplica la funcion a los elementos de la lista de derecha a izquierda
In: -fun : una funcion que recibe 2 argumentos
    -initVal: valor inicial
    -lst: la lista de valores para hacer fold
Out: El resultado de aplicar la funcion a los elemento de derecha a izquierda
|#


(print "EJERCICIO 2")
(newline)


(define (my-foldr fun initVal lst)
  (cond
    ((empty? lst) initVal)
    (else (fun (car lst) (my-foldr fun initVal (cdr lst))))))


; Test
 (test  (my-foldr + 0 '(1 2 3 4 5)) 15 )
 (test  (my-foldr + 0 '(4 3 2 1)) 10 )
 (test  (my-foldr - 0 '(1 2 3 4 5)) 3 )
 (test  (my-foldr - 0 '(4 3 2 1)) 2 )

(print "EJERCICIO 3")
(newline)

; Reject no conoce cual es el criterio de rechazo, asi que no depende de otra funcion, no necesito modificarla
; para poder ejecutar otro criterio, esto a nivel de codigo es una buena practica.



#|
Contract :reject recibe una lista y una funcion de exclusion (condicion)
         retorna una nueva lista con los elementos que no satisfacen la condicion
In: -lst : una lista de elementos
    - condFun: una funcion que reciba un elemento  
Out: Lista resultante de excluir los elementos de la lista original
|#

  (define (reject lst condFun)
  (filter (lambda (x) (not (condFun x))) lst))

(define lst '(1 2 3 4 5))

(test (reject lst even?) '(1 3 5))
(test (reject lst odd?) '(2 4))
(test (reject lst (lambda (x) (eq? x 5))) '(1 2 3 4))





(print "EJERCICIO 4")
(newline)



#|
Contract: merge recibe una lista de numeros y devulve la lista ordenada usando merge sort
In: lst : la lista de numeros a ordenar
Out: la lista ordenada
|#

  
(define (mergesort lst)
  (define (merge left right)
    ; funcion de soporte para unir 2 listas
    (cond
      ((empty? left) right)
      ((empty? right) left)
      ((< (car left) (car right))
       (cons (car left) (merge (cdr left) right)))
      (else (cons (car right) (merge left (cdr right))))))

  (define (split lst)
    ; funion que divide la lista en mitades
    (define len (length lst))
    (define pivot (quotient len 2))
    (list (take lst pivot) (drop lst pivot)))

  (cond
    ((<= (length lst) 1) lst) ; caso base: la lista ya esta ordenada
    (else
     (let* ((split-lists (split lst))
            (left (car split-lists))
            (right (cadr split-lists)))
       (merge (mergesort left) (mergesort right))))))

; Test
(test (mergesort '(23 -4 0 4 2 347 -21 3)) '(-21 -4 0 2 3 4 23 347))



(print "EJERCICIO 5")
(newline)


#|
Contract: merge recibe una lista y una funcion de comparacion para ordenar la lista
In: -lst: la lista para ordenar
    -compFun: la funcion de comparacion
Out: El resultado de unir ambas listas en una sola lista ordenada
|#

(define (custom-mergesort lst fun-comp)
  (define (merge left right)
    (cond
      ((empty? left) right)
      ((empty? right) left)
      ((fun-comp (car left) (car right))
       (cons (car left) (merge (cdr left) right)))
      (else (cons (car right) (merge left (cdr right))))))

  (define (split lst)
    (define len (length lst))
    (define pivot (quotient len 2))
    (list (take lst pivot) (drop lst pivot)))

  (cond
    ((<= (length lst) 1) lst)
    (else
     (let* ((split-lists (split lst))
            (left (car split-lists))
            (right (cadr split-lists)))
       (merge (custom-mergesort left fun-comp) (custom-mergesort right fun-comp))))))

; con  <
(test (custom-mergesort '(23 -4 0 4 2 347 -21 3) <) '(-21 -4 0 2 3 4 23 347))

;con  >
(test (custom-mergesort '(23 -4 0 4 2 347 -21 3) >) '(347 23 4 3 2 0 -4 -21))








(print "EJERCICIO 6")
(newline)

#|

Contract: devuelve una función que actúa como un comparador.
          la función compara si un número es mayor que n
In:   n : el numero limite para la comparacion
Out: Una funcion que toma un numero como parametro
     devuelve #t si el numero es mayor a 'n' y  #f si no.
|#


(define (more-than n)
  (lambda (x) (> x n)))

; Test
(define greater-than-10 (more-than 10))

(test (filter (more-than 10) '(13 2 31 45 9 10)) '(13 31 45))


(print "EJERCICIO 7")
(newline)


#|
<binary-tree> ::= Empty | Node <binary-tree> Number <binary-tree>
<max-bt> ::= "max-bt" "(" <binary-tree> ")"  

|#
























