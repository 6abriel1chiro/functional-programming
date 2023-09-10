#lang racket

#| 

(define (random-change lst)
  (if (empty? lst)
      '() ; return empty list if input is empty
      (let ([len (length lst)] ; get the length of the list
            [new-val (random 100)]) ; generate a random value between 0 and 99
        (list-set lst (random len) new-val)))) ; replace a random element with the new value


(define my-list '(1 2 3 4 5)) ; define a sample list
(random-change my-list) ; call the function on the list


 
(define (randomly-change-vector-element vec)
  (vector-set! vec
               (random (vector-length vec))
               (+ (vector-ref vec (random (vector-length vec)))
                  (random 10))))

; Example usage
(define my-vector (vector 1 2 3 4 5))
(display "Original vector: ")
(display my-vector)
(newline)

(randomly-change-vector-element my-vector)
(display "Vector after random change: ")
(display my-vector)
(newline)




(define my-list '(1 2 3 4 5)) ; define a sample list
(define len (length my-list)) ; define a global variable for the length of the list
(define index 0) ; define a global variable for the index of the list
(define old-val 0) ; define a global variable for the old value at the index
(define new-val 0) ; define a global variable for the new value at the index



(define (random-change-3 lst)
  (if (empty? lst)
      '() ; return empty list if input is empty
      (begin
        (set! index (random len)) ; update the index with a random number between 0 and len-1
        (set! old-val (list-ref lst index)) ; update the old value with the value at the index
        (set! new-val (+ old-val (random 10))) ; update the new value by adding a random number between 0 and 9 to the old value
        (list-set lst index new-val)) )) ; replace the old value with the new value in the list

(random-change-3 my-list) ; call the function on the list
|#




(define my-vec '(2 4 7 12 6)) 
(define len (length my-vec)) ;length of the list
(define index 0) ;index of the list
(define old-val 0) 
(define new-val 0) 



(define (replace-rnd-val lst)
  (cond ((empty? lst) '())
        (else (let ((len (length lst))
              (index (random len))
              (old-val (list-ref lst index))
              (new-val (+ old-val (random 10)))
              )
        (list-set lst index new-val)
        )
      )
   )
)

(replace-rnd-val my-vec)


