#lang play

; by guy steele
"holi"
; this is a comment
#|
this is a comment
|#

pi

4/3

; functions
(+ 1 4)

(* 1 2 3 4 5)
(and #t #f)


(and 2 3 7 1)
(or 2 3 4 1)

(string-append "wenos " "dias")

(number? 2)

(number? "hello")


(printf "operacion\n")

(sqrt -1/4)
(define mysqrt (sqrt -1/4))

(printf (number->string mysqrt))
(printf "\n")

(if (> 10 4 )
    "yes"
    #f)

(cond [ (> 10 4) "hola"]
      [ (< 3 5) "bye"]
      [else #f]
      )

; identificadores o variables

(define x 10)
x
(let ([x 2] [y 3]) (+ x y))

(define (double x)
(+ x x))

(double x)
  





(define (isPair x)
(if  (eq? (modulo x 2) 0)
#t
#f))

(isPair x)
  
; pares o simbolos ' (quote)

(cons 1 2)
(car (cons 1 2))

(cdr (cons 1 2))

; listas
empty

(cons 1 empty)
(cons 2 (cons 1 empty))

(printf "simplifying")
(list 1 2 3 4 (+ 4 1))

(printf "not simplifying")
'(1 2 3 4 (+ 4 1))

; loops

;append cons
;define a function named (add-pair-n pair n)
; i.e  '(1 . 2) 5 -> '(6 . 7)

(define (add-pair-n pair n)
  (cons (+ (car pair) n)
        (+ (cdr pair) n)))

(define (test actual expected)
  (if (equal? actual expected)
      "Test passed"
      (format "Test failed. Expected ~a but got ~a" expected actual)))

(displayln (test (add-pair-n (cons 1 2) 5) (cons 6 7)))


;(test (add-pair-n (cons 1 2) 5) (cons 6 7))

























