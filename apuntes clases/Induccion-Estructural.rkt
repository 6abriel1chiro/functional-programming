#lang play
#|
--> G. Peano
<nat> ::=  0 
          | (+ 1 <nat>)

--> Principio de razonamiento
--> INDUCCION

P(n) cierto para todo nat

P(0), para cero

P(n) => P(n+1)

o bien,

P(n - 1) => P(n)

=> programar ~= razonar
=> principio de procesamiento

para definir f que funciona para cualquier nat, necesitamos lo mismo
que necesitamos para razonar sobre nat.

(define (f n)
    (if (zero? n) ...
        ... (f (sub1 n)) ...))

|#


(define (fact n)
    (if (zero? n) 1
        (* n (fact (sub1 n)))))
; Esta es la forma intrinseca de usar recursion en los naturales

; => induccion ~= recursion
; Curry Howard correspondence
#|

<list> :=   (list)
          | (cons <val> <list>)

<val> := cualquier valor

(define (f lst)
  (match lst
     [empty ...] ; P('())
     [(cons head tail) ... head ... (f tail) ...) ; caso recursivo]
))


|#


(define (len lst)
  (match lst
     [(list) 0] ; P('())
     [(cons h t) (+ 1 (len t))]
  ))

(define l (list 1 2 3 4))
(len l)


; contains? :: lista -> boolean
; determina si la lista contiene el elemento

(define (contains? lst elem)
  (match lst
     [(list) #f] 
     [(cons head tail) (if (eq? head elem)
                           #t
                           (contains? tail elem))]
 ))

(test (contains? l 1) #t)
(test (contains? l 7) #f)
(test (contains? '() 5) #f)

; tarea, define bajo la misma idea: sum, reverse, map, foldl


#|
<bintree> ::=   <val>
              | (list <val> <bintree> <bintree>


(define (f bt)
     (match bt
        [(leaf val) ...] ; caso base
        [(node val left right) ... (f left) ... (f right))]
|#

(deftype BinTree
  (leaf value)
  (node value left right)
  )

; containsBT? :: bt val -> boolean
; determina si val esta en el BT


(define (containsBT? bt val)
    (match bt
       [(leaf v) (if (eq? val v) #t #f )] 
       [(node v left right) (or (eq? val v)
                                (containsBT? left val)
                                (containsBT? right val))]
      ))

(test (containsBT? (leaf 1) 2)#f)
(test (containsBT? (leaf 1) 1) #t)
(test (containsBT? (node 4 (leaf 5) (node 3 (leaf 1) (leaf 2))) 1) #t)
(test (containsBT? (node 4 (leaf 5) (node 3 (leaf 1) (leaf 2))) 7) #f)
