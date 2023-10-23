#lang play



(define (isEven n)
    (match n
      [0 #t]
      [1 #f]
      [_ (odd (sub1 n))]
      )

  )

(define (odd n)
    (match n
      [0 #t]
      [1 #f]
      [_ (isEven (sub1 n))]
      )
  )
