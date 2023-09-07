#lang play

; car -> first
; cdr -> rest

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

(test (add-pair-n (cons 1 2 ) 5) (cons 6 7))


(define (sumL lst)
  (cond
    [(empty? lst) 0]
    [else (+ (car lst) (sumL (cdr lst)))]
    )
  )

(sumL '(1 2 3))

; formas de escribir una lista

'(1 2 3 4 )

(list 1 2 3 4)

(cons 1 (cons 2 empty)); '() list




;replace
(print "tarea")
(newline)

(define (replace old-val new-val lst)
  (cond
    [(empty? lst) '()]
    [(equal? old-val (car lst)) (cons new-val (cdr lst))]
    [else (cons (car lst) (replace old-val new-val (cdr lst)))]))

(test (replace 1 5 '(3 1 2)) '(3 5 2))


; vectors




(def vec (vector 1 2 3 4))
vec
(vector-set! vec 3 3)
vec




; lamda calculo
; map

(define lst (list 1 2 3 4))

(define (add2 n) (+ n 2))

(map add2 lst)

; funciones que aceptan otras funciones
;map
(define planets (list "earth" "mars" "neptune" "jupiter"))


(map string-length planets)
; reduce

(foldl cons '() '(1 2 3))



(foldr cons '() '(1 2 3))

; custom


(define (reject lst fun)
  (cond [(empty? lst) '()]
        [(if (fun (car lst))
             (reject (cdr lst) fun)
             (cons (car lst) (reject (cdr lst) fun)))]
        ))


; funciones anonimas

(reject '(1 2 3) (λ (x) (eq? x 3)
        ))

;λ

; funciones que devuelven funciones


(define (addN n)
  (λ (x) (+ x n)))

(addN 78)

(map (addN 78) lst )


(map (addN 124) lst )


; currificacion



























































