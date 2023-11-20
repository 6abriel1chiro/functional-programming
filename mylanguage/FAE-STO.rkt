#lang play
(print-only-errors #f) ; Para ver solo los errores.

#|
<FAE> ::=   <num> | <bool> | <id>
            | (+ <FAE> <FAE>)
            | (- <FAE> <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>)
            | (fun <id> <FAE>) 
            ; cajas mutables
            | (newbox <expr>)
            | (setbox <expr> <expr>)
            | (openbox <expr>)
            | (seqn <expr> <expr>) 
|#

; Ejemplo de box
#|


Ahi si hay problemas con los argumentos, podemos hacer el sistema de tipos. 
|#

(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [add l r]                               ; (+ <FAE> <FAE>)
  [sub l r]                               ; (- <FAE> <FAE>)
  [if-tf c et ef]                         ; (if-tf <FAE> <FAE> <FAE>)
  [with id-name named-expr body-expr]     ; (with <id> <FAE> <FAE>)
  [id name]                               ; <id> 
  [app fname arg-expr]                    ; (app <FAE> <FAE>) ; ahora podemos aplicar una funcion a otra
  [fun arg body]                          ; (fun <id> <FAE>) ; mantenemos el <id> como el nombre del argumento
  [newbox b]
  [openbox b]
  [setbox b val]
  [seqn e1 e2]
) 


; 1. Actualizar el environment
#|
<env> ::= (mtEnv)
          | (aEnv <id> <loc> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id loc env)
  )

; empty-env -> (mtEnv)
(define empty-env (mtEnv))

; extend-env:: <id> <loc> <env> -> <env>
(define extend-env aEnv)
; env-lookup :: <id> <loc> -> <val>
; buscar el valor de una variable dentro del ambiete
(define (env-lookup x env)
  (match env
    [(mtEnv) (error "undefined: " x)]
    [(aEnv id loc tail)(if (eq? id x) loc (env-lookup x tail))]
    )
  )


; 2. Crear el store
#|
<sto> ::= (mtSto)
          | (aSto <loc> <val> <sto>)
|#

(deftype Sto
  (mtSto)
  (aSto loc val sto)
  )

(define empty-sto (mtSto))
(define extend-sto aSto)
(define (sto-lookup l sto)
  (match sto
    [(mtSto) (error "segmentation fault: " l)]
    [(aSto loc val tail) (if (eq? l loc) val (sto-lookup l tail))]
    )
  )

; parse: Src -> Expr
; parsea codigo fuente
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '+ s1 s2) (add (parse s1) (parse s2))]
    [(list '- s1 s2) (sub (parse s1) (parse s2))]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (app (fun x (parse b)) (parse e))]
    [(list 'newbox b) (newbox (parse b))]
    [(list 'openbox e) (openbox (parse e))]
    [(list 'setbox b v) (setbox (parse b) (parse v))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list arg e) (app (parse arg) (parse e))]
    [(list 'fun (list arg) body) (fun arg (parse body))]
    )
  )
; 4. Incluir un nuevo tipo de valor v*s
(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env
  (v*s val sto) ; val (valV/closureV) + sto (last memory)
  (boxV loc) ; Que contiene un box?
  )


; Que devuelve un newbox 10?
; Una caja, pero eso no lo soporta nuestro lenguaje


; 3. Actualizar interp.
; Descubrimiento clave de la clase pasada: Sto necesita existir
; interp :: Expr Env Sto -> Val*Sto --> CÃ3mo represento este valor?
; interpreta una expresion
(define (interp expr env sto)
  (match expr
    [(num n) (v*s (valV n) sto)] ; 6. Actualizar valores basicos
    [(bool b) (v*s (valV b) sto)]
    [(fun arg body) (v*s (closureV arg body env) sto)]
    [(id x) (v*s (sto-lookup (env-lookup x env) sto) sto)] ; 7. Actualizar la busqueda de variables
    [(add l r) ; 8. Actualizar operaciones
     ; 1. Evaluar l con el sto actual
     (def (v*s l-val l-sto) (interp l env sto))
     ; 2. Evaluar r con el sto modificado por l
     (def (v*s r-val r-sto) (interp r env l-sto))
     ; 3. Evaluar la suma
     (v*s (valV+ l-val r-val) r-sto)]
    
    [(sub l r) ; 1. Evaluar l con el sto actual
     (def (v*s l-val l-sto) (interp l env sto))
     ; 2. Evaluar r con el sto modificado por l
     (def (v*s r-val r-sto) (interp r env l-sto))
     ; 3. Evaluar la resta
     (v*s (valV- l-val r-val) r-sto)]
    
    [(if-tf c et ef)
     (def (v*s c-val c-sto) (interp c env sto))
     (if (valV-v c-val)
         (interp et env c-sto)
         (interp ef env c-sto))]
    ; 9. Actualizar la aplicacion de funcion
    [(app f e) ; f -> fun-expr & e -> arg-expr
     (def (v*s (closureV arg body fenv) fun-sto) (interp f env sto))
     (def (v*s arg-val arg-sto) (interp e env fun-sto))
     ; 0. Obtener nuevla loc, como?
     (def new-loc (malloc arg-sto))
     ; 1. Extender ambiente
     ; 2. Extender store
     ; 3. interp body
     (interp body
             (extend-env arg new-loc fenv)
             (extend-sto new-loc arg-val arg-sto))]

    [(newbox b)
     ; 1. Interpretar b
     (def (v*s b-val b-sto) (interp b env sto))
     ; 2. Nueva direccion
     (def new-loc (malloc b-sto))
     ; 3. Actualizar sto
     (v*s (boxV new-loc) (extend-sto new-loc b-val b-sto))
     ]

    [(openbox b)
     ; 1. interp b
     (def (v*s (boxV loc) b-sto) (interp b env sto))
     ; 2. devolver resultado
     (v*s (sto-lookup loc b-sto) b-sto)     
     ]

    [(setbox b v)
     ; 1. interp b --> loc que va a ser actualizado.
     (def (v*s (boxV loc) b-sto) (interp b env sto))
     ; 2. interp v --> para simplificar v
     (def (v*s v-val v-sto) (interp v env b-sto))
     ; 3. actualizar el sto
     (v*s v-val (extend-sto loc v-val v-sto))
     ]

    [(seqn e1 e2)
     ; interp e1
     (def (v*s e1-val e1-sto) (interp e1 env sto))
     ; interp e2 de modo que e1 afecte a e2
     (def (v*s e2-val e2-sto) (interp e2 env e1-sto))
     ; return
     (v*s e2-val e2-sto)
     ]
))

; malloc :: sto -> loc
; devuelve una nueva posicion de memoria en base a un sto dado
(define (malloc sto)
  (match sto
    [(mtSto) 0]
    [(aSto loc _ tail) (+ 1 (malloc tail))] ; usamos longitud de sto. 
    )
  )

; valV+ : Val -> Val
(define (valV+ s1 s2)
  (valV (+ (valV-v s1) (valV-v s2)))
  )

(define (valV- s1 s2)
  (valV (- (valV-v s1) (valV-v s2)))
  )

; run: Src -> Src
; corre un programa
(define (run prog) ; 10. Actualizar match
  (def (v*s res r-sto) (interp (parse prog) empty-env empty-sto)) ;5. Actualizar arg run
    (match res
      [(valV v) v]
      [(closureV arg body env) res]
      [(boxV v) res]
      )
    )

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
(test (run '{with {x 3} {if-tf {+ x 1} {+ x 3} {+ x 9}}}) 6)
(test/exn (run '{f 10}) "undefined")
(test (run '{with {f {fun {x} {+ x x}}}{f 10}}) 20)
(test (run '{{fun {x} {+ x x}} 10}) 20)
(test (run '{with {add1 {fun {x} {+ x 1}}}{add1 {add1 {add1 10}}}}) 13)
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {x} {+ {add1 x} {add1 x}}}}
                        {foo 10}}}) 22)
(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {f} {+ {f 10} {f 10}}}}
                        {foo add1}}}) 22)
(test (run '{{fun {x}{+ x 1}} {+ 2 3}}) 6)
(test (run '{with {apply10 {fun {f} {f 10}}}
                  {with {add1 {fun {x} {+ x 1}}}
                        {apply10 add1}}}) 11)


(test (run '{with {addN {fun {n}
                       {fun {x} {+ x n}}}}
            {{addN 10} 20}}) 30)



(test (run '{with {b {newbox 10}}
                  {seqn
                   {setbox b 20}
                   {openbox b}}}) 20)

(test (run '{with {make-box {fun {x} {newbox x}}}
      {setbox {make-box 10} 20}
}) 20)

(test (run '{with {b {newbox 10}}
  {seqn
       {setbox b {+ 1{openbox b}}}
       {openbox b}}
}) 11)

(test/exn (run '{with {a {newbox 0}}
            {seqn {with {b 3} b}
                  b}}) "undefined")

(test (run '{with {a {newbox 0}}
            {with {f {fun {x} {+ x {openbox a}}}}
                  {seqn
                   {setbox a 2}
                   {f 5}}}}) 7)