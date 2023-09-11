#lang play
  ; Contract: merge takes two sorted lists and merges them into a single sorted list.
  ; Input:   left - a sorted list
  ;          right - another sorted list
  ; Output:  A sorted list containing all elements from left and right.
  
(define (merge left right)
  (define (merge-helper left right acc)
    (cond
      ((empty? left) (append (reverse acc) right))
      ((empty? right) (append (reverse acc) left))
      ((< (car left) (car right))
       (merge-helper (cdr left) right (cons (car left) acc)))
      (else (merge-helper left (cdr right) (cons (car right) acc)))))
  
  (merge-helper left right '()))

(define (mergesort lst)
  (cond
    ((<= (length lst) 1) lst)
    (else
     (let* ((mid-point (/ (length lst) 2))
            (left (take lst mid-point))
            (right (drop lst mid-point)))
       (merge (mergesort left) (mergesort right))))))

; Test the mergesort function
(define unsorted-list '(23 -4 0 4 2 347 -21 3))
(define sorted-list (mergesort unsorted-list))
(test sorted-list '(-21 -4 0 2 3 4 23 347))
