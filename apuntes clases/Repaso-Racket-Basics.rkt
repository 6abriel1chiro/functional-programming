#lang play
; Mini repaso

#|
En general, definimos lo siguiente:

      eval(expr) --> val (prim/DS - objeto - funcion)

|#

; Scheme -> Racket
; PRIMITIVAS
1           ; numeros
2+1i        
"hola"      ; strings
'hola       ; symbol
#f          ; booleanos
#t

(printf "hola")  ; side effect

; FUNCIONES
; Funciones, <procedure>
+
eq?
; Para ejecutar una funcion, se debe utlizar ()
; ( nombreFuncion arg1 arg2 ... argN )
(eq? 2 3)

; CONDICIONALES
; if no es un keyword como tal, sino una funcion
; por eso, la invocamos de esta manera:
; (if condicion condicionTrue condicionFalse)

(if (zero? 0) "Hola" "Chao")

; Otra forma es usando la funcion cond
; (cond [condicion resultado] ... [else resultado])
(cond [(zero? 2) "Hola"]
      [(eq? 2 3) "Chao"]
      [else #f])

; VARIABLE + FUNCIONES
; Variables
; global
(define x 10)
x
;local
(let ([x 4])
  x)
x
; Para definir variables locales en funcion de otra variable local, usar let*

; Definir funciones
(define (run x) ; firma
  x ; cuerpo
  )

(run 3)

; ESTRUCTURAS DE DATOS
; Pares
; (cons elem1 elem2) --> '(elem1 . elem2)
(cons 2 3)
(car (cons 2 3)) ; acceder al primer elemento
(cdr (cons 2 3)) ; acceder al segundo elemento / ultimo elemento

; Listas
(define lst (list 1 2 3 4))
(car lst) ; first tambien puede usarse en lugar de car
(cdr lst) ; rest tambien puede usarse en lugar de cdr


; En general vamos a usar recursividad para recorrer listas
; Ejemplo. Tomar solo los impares de una lista de numeros
(define (pick-odds lista)
  (cond [(empty? lista) empty] ; caso base
        [(if (odd? (car lista)) ; caso recursivo
                   (cons (car lista) (pick-odds (cdr lista)))
                   (pick-odds (cdr lista)))]
        )
  )
(pick-odds lst)


; Pares +  Listas NO son MUTABLES -->
; No puedo cambiar sus valores.

; Vectores --> Son mutables

(define vec (vector 1 2 3 4))
vec
(vector-set! vec 0 4)
vec

; Sin embargo, cuidado, si trato de hacer esto mismo con un vector puesto de forma literal
; no va a funcionar, ese es el funcionamiento de los vectores. 
;(vector-set! '#(1 2 3 4) 0 4)
