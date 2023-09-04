#lang play
; 1. Deine una funcion recursiva que simule el comportamiento de
; map

#|
<list> ::=   (list)
           | (cons val <list>)

(define (fun lst)
  (match lst
    [(list) ...]
    [(cons head tail) ... head ... (fun tail)]
  )
)

|#


(define (my-map fun lst)
  (match lst
    [(list) '()]
    [(cons head tail) (cons (fun head) (my-map fun tail))]
  )
)

(test (my-map number->string '(1 2 3)) '("1" "2" "3"))
(test (my-map even? '(1 2 3)) '(#f #t #f))
(test (my-map odd? '()) '())

; 2. Define una funcion recursiva que simule el comportamiento de
; foldr

(define (my-foldr fun initVal lst)
  (match lst
    [(list) initVal]
    [(cons head tail) (fun head (my-foldr fun initVal tail))]
  )
)

(test (my-foldr cons '() '(1 2 3 4)) '(1 2 3 4))

; 3. Implementa la funcion reject vista en clase usando filter
; y funciones anonimas

; Funcionamiento de filter
(define (reject cond lst)
  (filter (λ (val) (if (cond val) #f #t)) lst))

; filter cond lst
; necesito recorrer la lista, entonces voy a poner una lambda
; por cada elemento de la lista, evalua con la funcion enviada
; y filtra con respecto a eso.


(test (reject odd? '(1 2 3 4 5 6)) '(2 4 6))

; 4. Mergesort
; divide y conquista --> problemas mas pequenos para resolver
; algo mas grande.

#|
           2 8 5 3 | 9 4 1 7

         2 8 | 5 3   9 4 | 1 7

      2 | 8    5 | 3   9 | 4   1 | 7

       2 8    3 5      4 9   1 7

         2 3 5 8        1 4 7 9

            1 2 3 4 5 7 8 9

|#

; merge: <list> <list> -> <list>
; devuelve una lista ordenada a partir de dos listas ordenadas
(define (merge lst1 lst2)
  (cond
    [(and (empty? lst1) (empty? lst2)) '()]
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (if (< (car lst1) (car lst2))
              (cons (car lst1) (merge (cdr lst1) lst2))
              (cons (car lst2) (merge lst1 (cdr lst2)))
              )]
    )
  )


(test (merge '() '()) '())
(test (merge '(1 2) '()) '(1 2))
(test (merge '() '(1 2)) '(1 2))
(test (merge '(5 6) '(1 2 4 8)) '(1 2 4 5 6 8))


; mergesort: <list> -> <sorted-list>
; devuelve una lista ordenada
(define (mergesort lst)
  (if (< (length lst) 2)
      lst ; --> Es una lista vacia o unitaria
      (let* ([pivot (quotient (length lst) 2)]
             [izquierdo (take lst pivot)]
             [derecho (drop lst pivot)])
        (merge (mergesort izquierdo) (mergesort derecho))
        ))
)

(test (mergesort '(1)) '(1))
(test (mergesort '()) '())
(test (mergesort '(2 8 5 3 9 4 1 7)) '(1 2 3 4 5 7 8 9))


; 5. mergesort generico


; merge-gen: <list> <list> -> <list>
; devuelve una lista ordenada a partir de dos listas ordenadas
(define (merge-gen lst1 lst2 fun)
  (cond
    [(and (empty? lst1) (empty? lst2)) '()]
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (if (fun (car lst1) (car lst2))
              (cons (car lst1) (merge-gen (cdr lst1) lst2 fun))
              (cons (car lst2) (merge-gen lst1 (cdr lst2) fun))
              )]
    )
  )


; mergesort-gen: <list> -> <sorted-list>
; devuelve una lista ordenada
(define (mergesort-gen lst pred)
  (if (< (length lst) 2)
      lst ; --> Es una lista vacia o unitaria
      (let* ([pivot (quotient (length lst) 2)]
             [izquierdo (take lst pivot)]
             [derecho (drop lst pivot)])
        (merge-gen (mergesort-gen izquierdo pred) (mergesort-gen derecho pred) pred)
        ))
)

(test (mergesort-gen '(1) >) '(1))
(test (mergesort-gen '() >) '())
(test (mergesort-gen '(2 8 5 3 9 4 1 7) <) '(1 2 3 4 5 7 8 9))
(test (mergesort-gen '(2 8 5 3 9 4 1 7) >) '(9 8 7 5 4 3 2 1))

; 6. Funcion de una funcion

; more-than n -> λ

(define (more-than n)
  (λ (x) (< n x))
 )

(test (map (more-than 10) '(1 3 40 50)) '(#f #f #t #t))
(test (filter (more-than 10) '(1 3 40 50)) '(40 50))

; 7. max bt
#|

<bintree> ::=   (leaf val)
              | (node val <bintree> <bintree>)


(define (fun bt)
  (match bt
     [(leaf v) ...]
     [(node v l r) ... v ... (fun l) ...(fun r)]
))

|#

(deftype Bintree
  [leaf val]
  [node val left right]
  )


(define (max-bt bt)
  (match bt
     [(leaf v) v] ; si hay solo raiz o he llegado a una hoja, entonces v
     [(node v l r) (max v (max-bt l) (max-bt r))]
))

(test (max-bt (leaf 3)) 3)
(test (max-bt (node 3 (leaf 7) (leaf 1))) 7)
(test (max-bt (node 3 (leaf 7) (node 1 (leaf 9) (leaf 2)))) 9)