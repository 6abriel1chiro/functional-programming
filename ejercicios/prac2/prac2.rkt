#lang play




#|
<Src> ::=   <number?>
          | {'+ <Src> <Src>}
          | {'- <Src> <Src>}
          | {'* <Src> ... <Src>}
          | {'- <Src>}

<Expr> ::=   (num <number?>)
           | (add <Expr> <Expr>)
           | (sub <Expr> <Expr>)
           | (mult <list>)
           | (neg <Expr>)
           | (gt <Expr> <Expr>)
           | (lt <Expr> <Expr>)
           | (if-tf <AE> <AE> <AE>)
           | (WITH <list> <WAE> )
|#


#|
<WAE> ::=   <num> | <bool> <id>
           | (+ <WAE> <WAE>)
           | (- <WAE> <WAE>)
           | (with <id> <WAE> <WAE>)
           | (withN <list> <WAE>)
           | (APP <id>  <WAE>)
|#



(deftype Expr
  [num n]
  [add l r]
  [sub l r]
  [mult vals]
  [neg n]
  [bool b]
  [if-tf c t f]
  [gt l r]
  [lt l r]
  [with x ne b]
  [id sym]
  [app fname arg]
  )
; <fundef> L= define (<id> <id>) <expr>
; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(cons '* vals) (mult (map parse vals))]
    [(list '- s1) (neg (parse s1))]
    [(list 'if-tf cond true-expr false-expr) (if-tf (parse cond) (parse true-expr) (parse false-expr))]
    [(list '> l r) (gt (parse l) (parse r))]
    [(list '< l r) (lt (parse l) (parse r))]
    [(list 'with (list id-name named-expr) body)
       (with id-name (parse named-expr) (parse body))]
    [(list 'withN assignments body)
     (parse (foldr (λ (assignment acc) `(with ,assignment ,acc)) body assignments))]
    )
  )

; subst: <id> <WAE> <WAE>  -> <WAE>
; substituye todas las apariciones del id en el cuerpo por el valor
(define (subst x v e)
  (match e
    [(num n) e]
    [(id sym) (if (eq? x sym) v e)]
    [(add l r) (add (subst x v l) (subst x v r))]
    [(sub l r) (sub (subst x v l) (subst x v r))]
    [(mult vals) (mult(map (λ (a) (subst x v a)) vals))]
    [(if-tf c t f) (if (interp c) (interp t) (interp f))]
    [(gt l r) (> (interp l) (interp r))]
    [(lt l r) (< (interp l) (interp r))]
    [(with y ne b)
     (if (eq? x y)
         (with y (subst x v ne) b)
         (with y (subst x v ne) (subst x v b)))
    ]
   )
 )
  


; interp :: Expr -> number?
; evalua una expresion aritmetica.

(define (interp expr)
  (match expr
    [(num n) n]
    [(id sym) (error "unidentified free variable: " sym)]
    [(bool b) b]
    [(add l r) (+ (interp l) (interp r))]
    [(sub l r) (- (interp l) (interp r))]
    [(mult vals) (foldl * 1 (map interp vals))]
    [(neg n) (- (interp n))]
    [(if-tf c t f) (if (interp c) (interp t) (interp f))]
    [(gt l r) (> (interp l) (interp r))]
    [(lt l r) (< (interp l) (interp r))]
    [(with x ne b) (interp(subst x (parse (interp ne)) b))]; {with {x ne} b}
  )
)





; run: Src -> Expr
; corre un programa
(define (run prog)
  (interp (parse prog))
  )

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{- 5 {+ 2 3}}) 0)
(test (run '{- 1}) -1)
(test (run '{- {+ 2 3}}) -5)
(test (run '{* 2 3 4}) 24)

(test (run '(if-tf #t (+ 1 1) (- 1 1)))2)

(test (run '(if-tf #f (+ 1 1) (- 1 1)))0)

(test (run '(if-tf  (+ 2 3) #t #f))#t)


(test (run '{if-tf (> 1 2) {+ 1 6} {- 1 1}}) 0)
(test (run '{if-tf (< 1 2) {+ 1 6} {- 1 1}}) 7)

(test (run '(> 5 3)) #t)
(test (run '(< 5 3)) #f)



(test/exn (run 'x) "unidentified")


#|
PRACTICA 2
1. * infinita
Modifica el lenguaje para que soporte multiplicación infinita y actualize el type checking (2pts)
(test (run '{* 1 1 1 1}) 1)
(test/exn (run '{* 1 #t 1 1}) "error: incorrect type")
|#

(test (run '{* 1 1 1 1}) 1)
;(test/exn (run '{* 1 #t 1 1}) "error: incorrect type")


#|

2. withN (3pts)
Actualmente, el lenguaje WAE soporta with con sólo una variable:
{with {x 2} {+ x 4}}

Para manejar dos variables, necesitamos anidar with
{with {x 2} {with {y 3} {with {z 1} {+ x {+ y z}}}}}

Haz las modificaciones necesarias para que pueda soportar  withN de la siguiente manera.
{withN {{x 2} {y 3} {z 1}} {+ x {+ y z}}}

Escribe pruebas para demostrar tu implementación
|#

; Test cases for withN 
(test (run '{withN {{x 2} {y 3} {z 1}} {+ x {+ y z}}}) 6)

(test (run '{withN {{x 2} {y 3}} {withN {{a 1} {b 4}} {+ x {+ y {+ a b}}}}}) 10)
(test (run '{withN {{x 2} {y 3}} {withN {{x 1} {z 5}} {+ x {+ y z}}}}) 9)
(test (run '{withN {{x 2}} x}) 2)
(test (run '{withN {} 42}) 42)

#|
3. free-vars (3pts)
Ahora que ya sabemos qué son las ocurrencias libres, también sabemos que si al interpretar nos encontramos con una de ellas
esperamos un error (pues esto quiere decir que no han sido substituidas). Crea la función (free-vars expr) que recibe una
expresión y devuelve la lista de ocurrencias libres de la expresión:


(test (free-vars (parse '{+ x {+ z 3}})) '(x z))
(test (free-vars (parse 'x)) '(x))
 Escribe pruebas para casos con with.


; free vars:: Expr -> list
;retorna una lista de variables de ocurrencias libres de una expresion

; Define the free-vars function
(define (free-vars expr)
  (match expr
    [(num _) '()] ; Numbers have no free variables
    [(bool _) '()] ; Booleans have no free variables
    [(id sym) (list sym)] ; An identifier is a free variable
    [(add l r) (append (free-vars l) (free-vars r))]
    [(sub l r) (append (free-vars l) (free-vars r))]
    [(mult vals) (foldl (λ (v acc) (append (free-vars v) acc)) '() vals)]
    [(neg n) (free-vars n)]
    [(if-tf c t f) (append (free-vars c) (append (free-vars t) (free-vars f)))]
    [(with x ne b) (append (remove-duplicates (free-vars b)) (remove x (free-vars ne)))]
   )
  )

; Helper function to remove duplicates from a list
(define (remove-duplicates lst)
  (if (null? lst) '()
      (cons (car lst)
            (remove-duplicates (filter (λ (x) (not (eq? x (car lst)))) (cdr lst))))))

; Helper function to remove an element from a list
(define (remove x lst)
  (filter (λ (y) (not (eq? x y))) lst))



; Testing  free-vars function
(test (free-vars (parse '{+ x {+ z 3}})) '(x z))
(test (free-vars (parse 'x)) '(x))
; Test case 1: Simple with expression
(test (free-vars (parse '{with {x 2} {+ x 4}})) '())

; Test case 2: Nested with expressions
(test (free-vars (parse '{with {x 2} {with {y 3} {with {z 1} {+ x {+ y z}}}}})) '())

; Test case 3: Free variable inside a with expression
(test (free-vars (parse '{with {x 2} {+ x y}})) '(y) )

; Test case 4: Free variable outside a with expression
(test (free-vars (parse '{with {x 2} {+ x {+ y 1}}} ) ) '(y ))

; Test case 5: Multiple free variables inside a with expression
(test (free-vars (parse '{with {x 2} {+ x {+ y z}}})) '(y z) )

  |#

#|
4. count-nums (2pts)
Sobre el AST, opera distintas funciones, en particular, las funciones de analyze recorren el árbol y extraen información o
alguna propiedad interesante. Ahora implementarás una función simple de analyze, (count-nums expr) que recorre una expresión
y devuelve la cantidad de constantes numéricas en la expresión.

(test (count-nums (add (num 3) (num 2))) 2)

Escribe pruebas para la función e impleméntala. 
|#


(define (count-nums expr)
  (match expr
    [(num _) 1] ; Si encontramos una constante numérica, devolvemos 1.
    [(add l r) (+ (count-nums l) (count-nums r))] ; Sumamos los resultados de las subexpresiones.
    [(sub l r) (+ (count-nums l) (count-nums r))] ; Similar para la resta.
    [(mult vals) (foldl (λ (v acc) (+ acc (count-nums v))) 0 vals)] ; Sumamos los resultados de la multiplicación.
    [(neg n) (count-nums n)] ; Consideramos el número dentro de la negación.
    [(bool _) 0] ; No contamos las constantes booleanas.
    [(if-tf c t f) (+ (count-nums c) (count-nums t) (count-nums f))] ; Sumamos los resultados de las ramas if.
    [(with _ ne b) (+ (count-nums ne) (count-nums b))] ; Sumamos los resultados de las subexpresiones de with.
    [(id _) 0] ; No contamos las variables.
   )
  )

; Pruebas
(test (count-nums (add (num 3) (num 2))) 2)
(test (count-nums (add (num 3) (sub (num 5) (num 1)))) 3)

(test (count-nums (parse '{with {x 2} {+ x {+ y 1}}})) 2)
;(test (count-nums (parse '{with {x 2} {+ x {+ y z}}})) 2)
;(test (count-nums (parse '{with {x 2} {+ x {+ y z}}})) 3)
