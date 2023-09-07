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

           | (WITH )
           | ()



|#
#|


<WAE> ::=   <num> | <bool> <id>
           | (+ <WAE> <WAE>)
 | (- <WAE> <WAE>)
 | (with <id> <WAE> <WAE>)
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
    [(with id ne b)
     (with id
           (subst x v ne)
           (if (eq? x id)
               b
               (subst x v b)
               ))]
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

#|

(* 3 2)
(* 1 2 3 4 5)


|#

(test (run '(if-tf #t (+ 1 1) (- 1 1)))2)

(test (run '(if-tf #f (+ 1 1) (- 1 1)))0)

(test (run '(if-tf  (+ 2 3) #t #f))#t)


(test (run '{if-tf (> 1 2) {+ 1 6} {- 1 1}}) 0)
(test (run '{if-tf (< 1 2) {+ 1 6} {- 1 1}}) 7)

(test (run '(> 5 3)) #t)
(test (run '(< 5 3)) #f)



(test/exn (run 'x) "unidentified")


#|
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


