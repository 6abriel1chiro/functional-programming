#lang play

; 4 - rember




(define (rember a lat)
  (cond ((empty? lat) lat)
        ((equal? a (car lat)) (cdr lat))
        (else (cons (car lat) (rember a (cdr lat))))
        )
  )


; RRESULTADO
 (rember 'mint '(lamb chops and mint jelly))




; 5 - insertR

(define my-list '("a" "b" "c" "d" "f" "g" "d" "h"))

(define (insertR new old lat)
(cond
  ((empty? lat) '()) ; si la lista esta vacia retorna lista una lista vacia
  ((equal? (car lat) old) ;si se encontro a old
   (cons (car lat)
         (cons new ; agregar new a la lista
               (cdr lat))) ; copiar el resto de la lista
   )
  (else (cons (car lat) ;si no es old, solo se sigue copiando la lista
              (insertR new old (cdr lat)))) ; iterar la lista con recursion
  )
  )

(test (insertR "e" "d" my-list) '("a" "b" "c" "d" "e" "f" "g" "d" "h"))
