#lang play


; ejer 2 prac 1.2

;Define una función recursiva que emule el comportamiento de foldr: (define (my-foldr fun initVal lst))


  
  ; Contract: my-foldr recibe una funcion, un valor inicial y una lista  y  aplica la funcion a los elementos de la lista de derecha a izquierda
  ; Input:   fun - una funcion que recibe 2 argumentos
  ;          initVal - valor inicial
  ;          lst - la lista de valores para hacer fold
  ; Output:  El resultado de aplicar la funcion a los elemento de derecha a izquierda

(define numbers '(1 2 3 4 5))

(foldr + 0 numbers)

(define (my-foldr fun initVal lst)
  (cond
    ((empty? lst) initVal)
    (else (fun (car lst) (my-foldr fun initVal (cdr lst))))))



; Test the my-foldr function
 (test  (my-foldr + 0 numbers) 15 )

