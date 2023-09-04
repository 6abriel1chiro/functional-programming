#lang play
; FUNCIONES
#|

Bien, recordemos un poco al lambda calculo:

Dentro de este, yo puedo definir una expresion (entidad de la que obtengo un valor al evaluar):

<expr> :=   x              --> variable
          | λx.<expr>      --> funcion
          | <expr>.<expr>  --> aplicacion de funcion

En esta definicion, las funciones estan al mismo nivel que las variables, son valores.

Que implica esto? Veamos tres usos.

|#

; FUNCTIONS AS PARAMETERS

; MAP
(define (add1 n)
  (+ n 1))

(define lst '(1 2 3 4 5))

; Hasta ahora, para aplicar la funcion add1 a todos los elementos de lst,
; tendriamos que usar recursividad y dentro de la definicion llamar a add1
(define (addL lista)
  (cond [(empty? lista) empty]
        [else (cons (add1 (car lista)) (addL (cdr lista)))] 
        ))

(addL lst)

#|
 map utiliza la funcion add1 como parametro (valido, pues una funcion es un valor en λ calculo)
 y la aplica a cada uno de los elementos de la lista.
(map fun '(n1 n2 ... nx)) --> '( (fun n1) (fun n2) ... (fun nx))

|#
(map add1 lst)

(define planets (list "earth" "mars" "neptune" "jupyter"))
(map string-length planets)

; FOLD L/R --> REDUCE
(foldl + 0 lst)
(foldr + 0 lst)
(foldr (λ (x y) (+ x y)) 0 lst)
#|
-> foldl
(list 1 2 3)
(+ 3 (+ 2 (+ 1 0)))

-> foldr
(list 1 2 3)
(+ 1 (+ 2 (+ 3 0)))
|#
; Desarrolla como foldl y foldr se ejecutan en las siguientes instrucciones
(foldl cons empty lst)
(foldr cons empty lst)


#|
Pero de que manera nos es util esto?

Nos ayuda a crear funciones que podemos reutilizar con mayor facilidad
por ejemplo, implementemos una funcion reject. 
|#

; reject lst fun

(define (reject lista fun)
  (cond [(empty? lista) empty]
        [(if (fun (car lista))
             (reject (cdr lista) fun)
             (cons (car lista) (reject (cdr lista) fun)))]
        ))
; Reject no conoce cual es el criterio de rechazo, asi que no depende de otra funcion, no necesito modificarla
; para poder ejecutar otro criterio, esto a nivel de codigo es una buena practica. 
(reject lst even?)
(reject lst odd?)

; FUNCIONES ANONIMAS -> LAMBDAS

; Pensemos en funciones como 5?, realmente vamos a utilizarla en mas lugares?
(define (5? num)
  (if (eq? num 5)
      #t
      #f))
(reject lst 5?)

; Puede que no, entonces, si yo necesito definir una funcion no asocida a un identificador
; puedo definir una funcion anonima de la misma manera que en el λ calculo
;  λ(x) (<expr>)
(reject lst (λ (x) (eq? x 5)))



#|
BNF --> Backus Naur Form
--> Gramatica libre de contexto
La gramatica ayuda a describir un lenguaje. 
<expr> :=  x    --> variables
         | λx.e --> funciones
         | e.e  --> aplicacion de funcion

<number> := 1 | 2 | 3

Siguendo el BNF del λ calculo, en Scheme hay las siguientes definiciones:
(define id val) --> variable

Pero en λ calculo no hay una definicion de funcion con nombre
(define (firma) body)
(define (foo arg) body) --> No existe en λ calc

Por eso, cuando en Racket definimos funciones, lo que realmente hace el lenguaje es asociar una funcion
anonima a un identificador.
(define x val)
(define foo (λ (arg) (body))
|#


; FUNCIONES QUE DEVUELVEN FUNCIONES
; func -> val
; func -> func?
; Diabolico no? Veamos como nos es util. 

(map add1 lst)

; (add124 number?) --> number
; add124 es una funcion que dado un numero
; devuelve la suma de este + 124

(define (add124 n)
  (+ 124 n))

(test (add124 1) 125)
(test (add124 -1) 123)
; add2
; add78
; add132354
(map (λ (x) (+ x 124)) lst)

(define (addN n)
  (λ (x)
    (+ x n)))

(addN 2)

(map (addN 2) lst)
(map (addN 78) lst)

; Cuando convierto una funcion A B -> C a A -> B -> C
; entonces estamos en un proceso de currificacion.

