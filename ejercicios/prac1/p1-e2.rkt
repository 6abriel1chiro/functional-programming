#lang play



(define (pow base exp)
  (if (= exp 0)
      1
      (* base (pow base (- exp 1)))))

(test (pow 2 4) 16 )




(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2))))))


(define (fibo n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibo (- n 1))
              (fibo (- n 2))))))

(test (fibo 0) 0)
(test (fibo 1) 1)
(test (fibo 2) 1)
(test (fibo 3) 2)
(test (fibo 4) 3)
(test (fibo 5) 5)
(test (fibo 6) 8)
(test (fibo 7) 13)
(test (fibo 8) 21)
