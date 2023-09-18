#lang play


; actualmente las funciones no son valores 
#|
<FAE> ::=    <num> | <bool> | <id>
          | (+ <FAE> <FAE>)
          | (- <FAE> <FAE>)
          | (if-tf <FAE> <FAE> <FAE>)
          | <id>
          | (with <id> <FAE> <FAE>)
          | (app <id> <FAE>) ; aplicaciom de funcion
          | (fun <id> <FAE> )

|#


; funciones de orden superior (funciones de primera clase )

(deftype Expr
  [num n]
  [bool b]
  [add l r]
  [sub l r]
  [if-tf c et ef]
  [with id-name named-expr body-expr]
  [id name]
  [eqN? x y]
  [evenN? x]
  [mult x y]
  [app fName arg]
  [fun arg body]
  )


#|
<env>::={mtEnv}
  |(<id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv id val env)
  )
;empty-env ->(mtEnv)
(define empty-env (mtEnv))
;extend-env::<id> <val> <env>-><env>
(define extend-env aEnv )
;env-lookup::<id> <env>-> <val>
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "Theres no such identifier")]
    [(aEnv id val nextEnv) (if (eq? x id) val (env-lookup x nextEnv))]
    
    ))


;funParse:src->expr
(define (fun-parse src)
(match src
  [(list 'define (list fname arg-name) body) ( fname arg-name (parse body))]
  )
  )
; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list 'eqN? x y) (eqN? (parse x) (parse y))]
    [(list 'evenN? x) (evenN? (parse x) )]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(list '* x y) (mult (parse x) (parse y))]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    ; {fun {x} body}
    [(list 'fun (list x) body) (fun x (parse body)) ]
    [(list f e)(app (parse f) (parse e))]
    
    )
  )



;subst :: id expr expr -> expr
;substituir x por v en e
(define (subst x v e)
  (match e
    [(num n) e]
    [(bool b) e]
    [(id sym) (if (eq? x sym) v e)]
    [(eqN? n1 n2) (eqN? (subst x v n1) (subst x v n2))]
    [(add l r) (add (subst x v l) (subst x v r))]
    [(evenN? n) (evenN? (subst x v n) )]
    [(mult l r) (mult (subst x v l) (subst x v r))]
    [(sub l r) (sub (subst x v l) (subst x v r))]
    [(if-tf c et ef) (if-tf (subst x v c) (subst x v et) (subst x v ef))]
    [(with id ne b)
     (with id (subst x v ne)
           (if (eq? x id)
               b
               (subst x v b)
               ))]
    [(app fname arg)(app fname (subst x v arg))]
   )
 )



; interp :: ExprEnv -> val
; with permite extender el environm

(define (interp expr  env)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (env-lookup x env)] ; buscar el valor de x
    [(mult x y) (* (interp x  env) (interp y  env))]
    [(eqN? x y) (= (interp x  env) (interp y  env))]
    [(evenN? n) (if (= (modulo (interp n  env) 2) 0) #t #f)]
    [(add l r) (+ (interp l  env) (interp r  env))]
    [(sub l r) (- (interp l  env) (interp r  env))]
    [(if-tf c et ef) (if (interp c  env)
                         (interp et  env)
                         (interp ef  env))]
    [(with x e b) ; {with {x e} b}
     (interp b  (extend-env x (interp e  env) env))]
    [(app f e)
     (def (fun arg body) (interp f env))
     (interp body  (extend-env arg (interp e env) mtEnv))
     ]
))
(define (count-fun expr )
  (match expr
    [(num n) 0]
    [(bool b) 0]
    [(id x) 0]
    [(mult x y) (+ (count-fun x ) (count-fun y ))]
    [(eqN? x y) (+ (count-fun x ) (count-fun y))]
    [(evenN? n) (if (= (modulo (interp n ) 2) 0) #t #f)]
    [(add l r) (+ (count-fun l ) (count-fun r ))]
    [(sub l r) (+ (count-fun l ) (count-fun r ))]
    [(if-tf c et ef) (+ (count-fun c )
                         (count-fun et )
                         (count-fun ef ))]
    [(with x e b) (+ (count-fun e) (count-fun b))]
    [(app fname arg)
     (+ 1 (count-fun arg))
     
     ]
))


; corre un programa
(define (run prog )
  
  (interp (parse prog)  mtEnv)
  )
;

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)

(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{with {x 3} {if-tf #t {+ x 3} {+ x 9}}}) 6)



(test (run '{eqN? 1 0}) #f)
(test (run '{* 2 3 }) 6)
(test (run '{eqN? 3 3}) #t)

(test (run '{evenN? 3}) #f)
(test (run '{evenN? 2}) #t)
(test (run '{evenN? 0}) #t)



;(run '{foo 10} (list {list 'define '(add1 x) '(+ x 1)} {list 'define '(foo x) '(+ (add1 x) (add1 x)) }))
;(run '{fact 4} (list {list 'define '(fact x) '(if-tf (eqN? x 0) 1 (* x (fact (- x 1))))} ))



