#lang play
(print-only-errors #t) ; Para ver solo los errores.

#|
<expr> ::=   <num> | <bool> | <id> | <string>
            | (+ <expr> <expr>)
            | (with <id> <expr> <expr>)
            | (app <id> <expr>) 
|#

#|
<FAE> ::=   <num> | <bool> | <id>
            | (prim op-name <FAE> ... <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento.
            | (dyn <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento. 
            | (fun <id> <FAE>) ; fun(que es una lambda) nombre-arg body
|#
; + - * / < <= >= > == !=
(define primitives
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '< <)
   (cons '> >)
   (cons '<= <=)
   (cons '>= >=)
   (cons '== eq?)
   (cons '!= (λ (x y) (not (eq? x y))))
   (cons '&& (λ (x y) (and x y)))
   (cons '|| (λ (x y) (or x y)))
   ))
; (apply (cdr (assq '+ primitives)) '(1 2 3 4))

(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [str s]                                 ; <string>
  [if-tf c et ef]                         ; (if-tf <FAE> <FAE> <FAE>)
  [with id-name named-expr body-expr]     ; (with <id> <FAE> <FAE>)
  [id name]                               ; <id> 
  [app fname arg-expr]                    ; (app <FAE> <FAE>) ; ahora podemos aplicar una funcion a otra
  [dyn fname arg-expr]
  [fun arg body]                          ; (fun <id> <FAE>) ; mantenemos el <id> como el nombre del argumento
  [prim name args]
  [nil]
) 


#|
<env> ::= (mtEnv)
          | (aEnv <id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val env)
  )

; empty-env -> (mtEnv)
(define empty-env (mtEnv))

; extend-env:: <id> <val> <env> -> <env>
(define extend-env aEnv)
; env-lookup :: <id> <env> -> <val>
; buscar el valor de una variable dentro del ambiete
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "undefined: " x)]
    [(aEnv id val tail)(if (eq? id x) val (env-lookup x tail))]
    )
  )

(define (refactor-name new-name old-name env)
  (match env
    [(mtEnv) env]
    [(aEnv id val tail) (if (eq? id old-name)
                            (aEnv new-name val tail)
                            (aEnv id val (refactor-name new-name old-name tail)))]
    )
  )

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? string?)(str src)]
    [(? symbol?) (if (eq? 'nil src) (nil) (id src))]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list 'dyn arg e) (dyn (parse arg) (parse e))]
    [(list arg e) (app (parse arg) (parse e))]; 2. Subir de nivel nuestras funciones
    [(list 'fun (list arg) body) (fun arg (parse body))] ; 1. Agregar el caso del fun
    [(cons prim-name args) (prim prim-name (map parse args))]
    )
  )

(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env
  )

; interp :: Expr  Env -> Val
; interpreta una expresion
(define (interp expr env)
  (match expr
    [(num n) (valV n)]
    [(bool b) (valV b)]
    [(str s)(valV s)]
    [(nil) (valV '())]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(prim prim-name args) (prim-ops prim-name (map (λ (x) (interp x env)) args))]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    [(with x e b) (interp b (extend-env x (interp e env) env))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] ; Por ahora, devolvemos la misma expresion que nos llego
    [(app f e)
     (def (closureV arg body fenv) (interp f env)) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body
    
     (interp body (extend-env arg (interp e env) fenv)) ; parece que no funciona ni con estatico ni dinamico
     ]
    [(dyn f e)
     (def (closureV arg body fenv) (interp f env)) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body    
     (interp body (extend-env arg (interp e env) env)) ; parece que no funciona ni con estatico ni dinamico
     ]
))

; prim-ops: op-name list[Val] -> Val
(define (prim-ops op-name args)
  (let ([vals (map (λ (x) (valV-v x)) args)])
    (valV (apply (cdr (assq op-name primitives)) vals))
    )
  )

; run: Src -> Src
; corre un programa
(define (run prog)
  (let ([res (interp (parse prog) empty-env)])
    (match res
      [(valV v) v]
      [(closureV arg body env) res])
    )
  )
#|
1. Extiende el lenguaje FAE con substitucion diferida para
soportar las operaciones:
                          + - * / < <= >= > == !=
|#
(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{+ 1 2 3 4}) 10)
(test (run '{* 2 3 4}) 24)
(test (run '{/ 12 2 2}) 3)
(test (run '{< 12 3}) #f)
(test (run '{<= 12 3}) #f)
(test (run '{< 12 12}) #f)
(test (run '{<= 12 12}) #t)
(test (run '{> 12 3}) #t)
(test (run '{>= 12 3}) #t)
(test (run '{> 12 12}) #f)
(test (run '{>= 12 12}) #t)
(test (run '{>= 12 12}) #t)
(test (run '{== 12 12}) #t)
(test (run '{== 12 11}) #f)
(test (run '{!= 12 12}) #f)
(test (run '{!= 12 11}) #t)

#|
2. Extiende el lenguaje FAE con las operaciones lógicas
and y or, asuma que funcione solamente para dos argumentos.
Que creen que pueda hacerse para que acepte argumentos
infinitos?
|#

(test (run '{&& 12 11}) 11)
(test (run '{&& #f #t}) #f)
(test (run '{|| #f #t}) #t)
(test (run '{|| 12 11}) 12)


#|
3. Constant Folding --> Esta es una optimización utilizada
en compiladores para eliminar el cálculo innecesario de
expresiones constantes y reemplazarlas directamente
con su valor. Considera que se calculan constantes en operaciones
puramente de variables. 
|#
; Ver si es que en la expresion hay variables
; contains-variables? list -> bool
; ve si hay variables en una expresion

(define (contains-variables? lst)
  (match lst
    [(list) #f]
    [(? num?) #f]
    [(? bool?) #f]
    [(? id?) #t]
    [(cons head tail) (if (contains-variables? head)
                          #t
                          (contains-variables? tail)
                          )
                          ]
    [(prim op args) (contains-variables? args)]
    )
  )

(test (contains-variables? (parse '{+ x x x x})) #t)
(test (contains-variables? (parse '{+ 1 3 4 5})) #f)
(test (contains-variables? (parse '{+ 1 x 4 5})) #t)
(test (contains-variables? (parse '{- 1 2 4 5})) #f)
(test (contains-variables? (parse '{- 1 2 {+ 4 5}})) #f)


;constant-folding: expr -> expr
; optimizacion de constant folding
(define (constant-folding expr)
  (match expr
    [(num n) expr]
    [(bool b) expr]
    [(nil) expr]
    [(id x) expr]
    [(prim prim-name args) (if (contains-variables? args)
                               (prim prim-name (map constant-folding args))
                               (let ([ans (interp (prim prim-name args) empty-env)])
                                 (match (valV-v ans)
                                   [(? number?) (num (valV-v ans))]
                                   [(? boolean?) (bool (valV-v ans))]
                                   )
                                 ))]
    [(if-tf c et ef) (if-tf (constant-folding c)
                         (constant-folding et)
                         (constant-folding ef))]
    [(with x e b) (with x (constant-folding e) (constant-folding b))] 
    [(fun arg body) (fun arg (constant-folding body))] 
    [(app f e) (app (constant-folding f) (constant-folding e))]
    )
  )

(test (constant-folding (parse '{with {x {+ 1 2 {* 3 4}}} {+ 1 x}}))
      (parse '{with {x 15} {+ 1 x}}))
(test (constant-folding (parse '{with {x {+ y 2 {* 3 4}}} {+ 1 x}}))
      (parse '{with {x {+ y 2 12}} {+ 1 x}}))
(test (constant-folding (parse '{if-tf {< x 1} {+ 3 3} {+ 5 9}}))
      (parse '{if-tf {< x 1} 6 14}))
(test (constant-folding (parse'{{fun {x} {+ x {* 2 4}}} {+ 5 5}}))
      (parse '{{fun {x} {+ x 8}} 10}))
(test (constant-folding

          (parse '{with {x 1} {fun {z} {+ x {- 1 {+ 2 3}}}}}))

          (parse '{with {x 1} {fun {z} {+ x -4}}}))

(test (constant-folding (parse '{with {z {+ 3 {* 5 4}}} z}))

                        (parse '{with {z 23} z}))



#|
4. Constant Propagation
Otra optimizacion en compiladores
es constant propagation. Cuando se reconoce
que un identificador es constante, entonces
se reemplazan sus ocurrencias inmediatamente
|#
; subs: AE --> AE ;Related to constant-propagation
(define (subst expr sym val)
  (match expr
    [(num n) expr]
    [(bool b) expr]
    [(with x e b) (with x (subst e sym val) (subst b sym val))]
    [(fun arg-sym body) (fun arg-sym (subst body sym val))]
    [(app fun-exp arg-exp) (app (subst fun-exp sym val) (subst arg-exp sym val))]
    [(prim prim-name args) (prim prim-name (map (λ (x) (subst x sym val)) args))]
    [(id x) (if (symbol=? x sym) val expr)]
    ))
(test (subst (parse '{+ 1 x x x}) 'x (num 2)) (parse '{+ 1 2 2 2}))
(test (subst (parse '{with {y x} {+ x x y}}) 'x (num 2))
      (parse '{with {y 2} {+ 2 2 y}}))

; constant-propagation: expr -> expr
; optimizacion de constant-propagation
(define (constant-propagation expr)
  (match expr
    [(num n) expr]
    [(bool b) expr]
    [(nil) expr]
    [(id x) expr]
    [(prim prim-name args) (prim prim-name (map constant-propagation args))]
    [(if-tf c et ef) (if-tf (constant-propagation c)
                         (constant-propagation et)
                         (constant-propagation ef))]
    [(with x e b) (if (contains-variables? e)
                      (with x e (constant-propagation b))
                      (with x e (constant-propagation (subst b x e))))] 
    [(fun arg body) (fun arg (constant-propagation body))] 
    [(app f e) (app (constant-propagation f) (constant-propagation e))]
    )
  )

(test (constant-propagation
   (parse '{with {x 3} {+ x x}})) (parse '{with {x 3} {+ 3 3}}))
(test (constant-propagation
   (parse '{with {x 3} {with {y 5} {+ x y}}})) (parse '{with {x 3} {with {y 5} {+ 3 5}}}))
(test (constant-propagation
   (parse '{with {x 3} {with {y 5} {+ z z}}})) (parse '{with {x 3} {with {y 5} {+ z z}}}))
(test (constant-propagation (parse '{with {x 1} x}))

                            (parse '{with {x 1} 1}))

(test (constant-propagation (parse '{with {x 1} {fun {z} x}}))

                            (parse '{with {x 1} {fun {z} 1}}))

#|
5. Dead-Store Elimination
Esta es otra optimizacion que quita las variables que no tienen
un uso al haber sido declaradas.
|#
(define (isVariableUsed? x expr)
  (match expr
    [(num n) #f]
    [(bool b) #f]
    [(nil) #f]
    [(id sym) (if (eq? x sym) #t #f)]
    [(prim prim-name args) (foldl (λ (x y) (or x y)) #f (map (λ (arg) (isVariableUsed? x arg)) args))]
    [(if-tf c et ef) (and (isVariableUsed? x c)
                         (isVariableUsed? x et)
                         (isVariableUsed? x ef))]
    [(with arg e b) (and (isVariableUsed? x e) (isVariableUsed? x b))] 
    [(fun arg body) (isVariableUsed? x body)] 
    [(app f e) (and (isVariableUsed? x f) (isVariableUsed? x e))]
    )
  )
(test (isVariableUsed? 'x (parse '{+ x x x x})) #t)
(test (isVariableUsed? 'a (parse '{+ x y z q})) #f)

(define (dead-store-elimination expr)
  (match expr
    [(num n) expr]
    [(bool b) expr]
    [(nil) expr]
    [(id x) expr]
    [(prim prim-name args) (prim prim-name (map dead-store-elimination args))]
    [(if-tf c et ef) (if-tf (dead-store-elimination c)
                         (dead-store-elimination et)
                         (dead-store-elimination ef))]
    [(with x e b) (if (isVariableUsed? x b)
                      (with x e b)
                      b
                      )] 
    [(fun arg body) (if (isVariableUsed? arg body)
                        (fun arg (dead-store-elimination body))
                        body)] 
    [(app f e) (app (dead-store-elimination f) (dead-store-elimination e))]
    )
  )

(test (dead-store-elimination (parse '{with {x 3} {+ 1 2 3 4}})) (parse '{+ 1 2 3 4}))
(test (dead-store-elimination (parse '{fun {x} {+ 1 2 3 4}})) (parse '{+ 1 2 3 4}))
(test (dead-store-elimination (parse '{fun {x} {+ 1 2 x 4}})) (parse '{fun {x} {+ 1 2 x 4}}))

#|
6. Dyn keyword 
|#

(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {x} {+ {add1 x} {add1 x}}}}
                        {dyn foo 10}}}) 22)

(test (run '{with {n 1}
                  {with {f {fun {z} {+ n z}}}
                        {with {n 2}
                              {+ {dyn f 2} {f 2}}}}}) 7)


