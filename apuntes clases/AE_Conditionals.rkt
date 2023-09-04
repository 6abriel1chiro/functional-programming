#lang play
(print-only-errors #t) ; para ver solo tests que fallan

#|
<AE> ::=   <num> | <bool>
           | (+ <AE> <AE>)
           | (- <AE> <AE>)
           | (if-tf <AE> <AE> <AE>)
           | (< <AE> <AE>)
           | (> <AE> <AE>)

|#


(deftype Expr
  [num n]
  [bool b]
  [add l r]
  [sub l r]
  [gt l r]
  [lt l r]
  [if-tf cond if-true if-false]
  )

; parse : src -> expr
(define (parse src)
  (match src 
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '> l r) (gt (parse l) (parse r))]
    [(list '< l r) (lt (parse l) (parse r))]
    [(list 'if-tf c e1 e2) (if-tf (parse c) (parse e1) (parse e2))]
    )
  )

(test (parse '1) (num 1))
(test (parse '{+ 2 1}) (add (num 2) (num 1)))
(test (parse '{+ 2 {- 3 2}}) (add (num 2) (sub (num 3) (num 2))))
(test (parse '{if-tf 0 1 2}) (if-tf (num 0) (num 1) (num 2)))
(test (parse '{if-tf {- 1 1} {+ 1 2} 2}) (if-tf (sub (num 1) (num 1)) (add (num 1) (num 2)) (num 2)))

; interp : expr -> val
(define (interp expr)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(gt l r) (> (interp l) (interp r))]
    [(lt l r) (< (interp l) (interp r))]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(if-tf c e1 e2) (if (interp c)
                       (interp e1)
                       (interp e2))]
    ))

#|
Sin gt o lt, podriamos hacer un if0 para ver si cond se reduce
a cero en algun momento.

[(if0 c e1 e2) (if (zero? (interp c))
                              (interp e1)
                              (interp e2))]
|#


 

; run: src -> val
(define (run prog)
   (interp (parse prog))) 

(test (run '{if-tf 0 #t #f}) #t)
(test (run '{if-tf {+ 1 1} #t #f}) #t)
(test (run '{if-tf {+ 1 1} #t 2}) #t)
(test (run '{if-tf #f 3 2}) 2)

(test (run '{> 3 4}) #f)
(test (run '{if-tf {> 6 5} {+ 6 5} {- 6 5}}) 11)