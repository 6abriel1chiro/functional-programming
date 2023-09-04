#lang play

; Clase - Elementos Basicos

#|
  Esto es un comentario en bloque


  Similar a *//*

Para ejecutar el codigo dentro de este editor pueden utilizar CTRL/Command + R

|#

; PRIMITIVES
; Numbers
1
-3
4.02

; Casos especiales
6.02e+23
1+2i
4/3
6/8

; Booleanos
#t
#f

; Strings
"hola mundo"
; se aceptan casi todos los caracteres
"λx:(μα.α→α).xx"

; Simbolos
'hola

; FUNCIONES PREDETERMINADAS
; Aritmetica simple

(+ 2 3)
(* 2 4 5)
(+ 1 2 3 4 5)

; Anidar
(+ 1 ( - 3 4))
; Use parenthesis wisely
(and #t #f)
(and 2 3)
(or 1 #f)

; Casting + Strings
(string-append "hola" " mundo")
(substring "Apple" 1 3)
(string->symbol "this")

; Printing
(printf "this~n")

; Imprima la raiz cuadrada de -1/4

; CONDICIONALES
#|

if (cond)
   ifTrue
   ifFalse

(if cond ifTrue ifFalse)
|#

(if (> 10 4)
    "hola"
    (if (< 3 5)
        "chao"
        #f))

; Para anidar usar cond
(cond [(> 10 4) "hola"]
      [(< 3 5) "chao"]
      [else #f])


(define num 10)

(if (eq? (modulo num 2) 0)
    (printf "Par~n")
    (printf "Impar~n"))

; Nombres de variables poco aceptables, pero posibles
(define slkdfjsljk~@!#!@$@%@#$!$#%@#$ 3)
slkdfjsljk~@!#!@$@%@#$!$#%@#$


; Variables locales
(define x 10)
(let ([x 3]
        [y 2])
    (+ x y))
; let oculta la variable global
; Cuando acaba, vuelve a ser global

(let* ([a 3]
      [b (+ a a)])
    (+ a b))

; anidar lets (metodo largo)
(let ([a 3])
  (let ([b (+ a a)])
    (+ a b)))

; Funciones
(define (double x)
  (+ x x))

(double 4)


(define (isEven? num)
  (if (eq? (modulo num 2) 0)
    #t
    #f))

(test (isEven? 2) #t)
(test (isEven? 7) #f)
