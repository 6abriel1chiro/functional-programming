#lang play
;Define una funcion (xx lst) que recibe una lista de
;booleanos y devuelve si #t si todos son booleanos.


(define (isBool lst)
  (andmap (λ (x) (boolean? x)) lst))



(test (isBool '(#t)) #t)
(test (isBool '(#t #f #t)) #f)
(test (isBool '()) #t)
(test (isBool '(#f)) #f)



(define (allTrue list)
  (foldr (λ (val1 val2) (and val1 val2)) #t list

   )
  )

(test (allTrue '(#t)) #t)
(test (allTrue '(#t #f #t)) #f)
(test (allTrue '()) #t)
(test (allTrue '(#f)) #f)

(define (yyyy lst fun)
   (foldr (lambda (a b) (cons (fun a) b)) '() lst))

(yyyy '(1 2 3) (lambda (x) (+ x 1)))

