#lang play
;Define una funcion (rango from to) que devuelva una lista
;con el rango inclusivo de los argumentos.

;(rango 3 7) --> '(3 4 5 6 7)
(define (rango from to)
  (cond
    ((not (integer? from)) '())
    ((not (integer? to)) '())
    ((> from to) '())         
    (else (build-list (+ (- to from) 1)
                      (lambda (i) (+ from i))))))


(rango 3 7)
(rango 7 3)
(display (rango "a" 7)) 



;Usando esa funcion, defina una funcion (primo? num) que
;devuelva si un numero es primo o no.



(define (primo? num)
  (cond
    ((not (integer? num)) #f) 
    ((<= num 1) #f)       
    ((= num 2) #t) 
    (else   (let* ([list (rango 2 (- num 1))] [valids (filter (λ (val) (= 0 (modulo num val)))list)])
    (empty? valids) )    )))


 (primo? 2) 
 (primo? 7) 
 (primo? 10)
 (primo? -3)
 (primo? 10000000)

