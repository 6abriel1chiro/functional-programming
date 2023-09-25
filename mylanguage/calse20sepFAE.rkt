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

; F1WAE VS FAE : FAE ACEPTA FUNCIONES COMO VALORES


; funciones de orden superior (funciones de primera clase )

(deftype Expr
  [num n]
  [bool b]
  [if-tf c et ef]
  [with id-name named-expr body-expr]
  [id name]
  [eqN? x y]
  [evenN? x]
  [mult x y]
  [app fName arg]
  [fun arg body]
  [primitive name args]
  )

;assq busca x en list
; apply : toma una funcion  y la aplica dentro de una list ejempli factorial
(define primitives
   (list
   (cons '+ +)
   (cons '- -)
    )
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

    [(list '* x y) (mult (parse x) (parse y))]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list f e)(app (parse f) (parse e))]
    ; {fun {x} body}
    [(list 'fun (list x) body) (fun x (parse body))]
    [(cons prim-name args) (primitive prim-name (map parse args))]
    
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
 
    [(evenN? n) (evenN? (subst x v n) )]
    [(mult l r) (mult (subst x v l) (subst x v r))]
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

(deftype val
  (valV v)
  (closureV arg body env) ; closure = fun + env
  )

; interp :: ExprEnv -> val
; with permite extender el environment
;interpreta expresiones
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(id x) (env-lookup x env)] ; buscar el valor de x
    [(mult x y) (* (interp x  env) (interp y  env))]
    [(eqN? x y) (= (interp x  env) (interp y  env))]
    [(evenN? n) (if (= (modulo (interp n  env) 2) 0) #t #f)]
    [(primitive prim-name args) (prim-ops prim-name (map (λ (x) (interp x) args )))]
    [(if-tf c et ef) (if (interp c  env)
                         (interp et  env)
                         (interp ef  env))]
    [(with x e b) ; {with {x e} b}
     (interp b  (extend-env x (interp e env) env))]
    [(fun arg body) (closureV arg body env)]
    [(app f e)
     (def (closureV arg body fenv) (interp f env))
     (interp body  (extend-env arg (interp e env) fenv)) ;mantenemos el scope estatico
     ]
))

;apply
;prim-ops
(define (prim-ops name args)
   (let ([vals (map (λ (x) (valV-v x)) args)])
     (valV (apply (cdr {assq name primitives}) vals))
     )
  )

#|
;valV+ : valV valV -> valV
(define (valV+ s1 s2)
  (valV (+ (valV-v s1) (valV-v s2)))
  )

;valV- : valV valV -> valV
(define (valV- s1 s2)
  (valV (- (valV-v s1) (valV-v s2)))
  )
|#

; corre un programa
(define (run prog )
  (let ([ res (interp (parse prog)  empty-env)])
    (match res
      [(valV v) v]
      [(closureV arg body env) res]
      )
  ))

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



(run '{with {foo {fun {x} x}}   {foo 3}})



(run '{with {add1 {fun {x} {+ x 1}}}
        {with {apply10 {fun {f} {f 10}}}
              {apply10 add1}}})


{run '{{fun {x} {+ x 2}}  10}}

(run '{with {addN {fun {n}
                       {fun {m}
                            {+ m n}}}}
                  {{addN 10} 20}})


