#lang play








#|
Práctica 3 - FAE

Nombre: Gabriel Ichiro Balderrama vargas

Asignatura: Programación Funcional

 
|#



#|
<expr> ::=   <num> | <bool> | <id>
            | (+ <expr> <expr>)
            | (with <id> <expr> <expr>)
            | (app <id> <expr>) 
|#

#|
<FAE> ::=   <num> | <bool> | <id>
            | (prim op-name <FAE> ... <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)ß
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento. 
            | (fun <id> <FAE>) ; fun(que es una lambda) nombre-arg body
|#
(define primitives
  (list
   (cons '+ +)
   (cons '- -)
   (cons 'and (λ (x y) (and x y)))
   (cons 'or (λ (x y) (or x y)))
   (cons 'concat string-append)
   ))
; (apply (cdr (assq '+ primitives)) '(1 2 3 4))

;procedure
;(string-append str ...) → string?
; str : string?
;from racket documentation
;https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29

(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [if-tf c et ef]                         ; (if-tf <FAE> <FAE> <FAE>)
  [str s]
  [with id-name named-expr body-expr]     ; (with <id> <FAE> <FAE>)
  [id name]                               ; <id>
  [dynamic-app fname arg-expr]            ; (dynamic-app <FAE> <FAE>) ahora podemos aplicar funciones con scope dinamico
  [app fname arg-expr]                    ; (app <FAE> <FAE>) ; ahora podemos aplicar una funcion a otra
  [fun arg body]                          ; (fun <id> <FAE>) ; mantenemos el <id> como el nombre del argumento
  [prim name args]
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




; parse: Src -> Expr
; parsea codigo fuente

(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(? string?) (str src)]
    [(list 'if-tf c et ef) (if-tf (parse c) (parse et) (parse ef))]
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list arg e) (app (parse arg) (parse e))]; 2. Subir de nivel nuestras funciones
    [(list 'dynamic arg e) (dynamic-app (parse arg) (parse e))]
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
    [(str s) (valV s)]
    [(id x) (env-lookup x env)]; buscar el valor de x en env
    [(prim prim-name args) (prim-ops prim-name (map (λ (x) (interp x env)) args))]
    [(if-tf c et ef) (if (interp c env)
                         (interp et env)
                         (interp ef env))]
    [(with x e b) (interp b (extend-env x (interp e env) env))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] ; Por ahora, devolvemos la misma expresion que nos llego
    [(dynamic-app f e)
     (def (closureV arg body _) (interp f env)) 
     (interp body (extend-env arg (interp e env) env))
     ]
    [(app f e)
     (def (closureV arg body fenv) (interp f env)) ; Esto permite encontrar (fun 'x (add (id 'x) (id 'x))) por ejemplo y tomar arg y body
    
     (interp body (extend-env arg (interp e env) fenv)) ; parece que no funciona ni con estatico ni dinamico
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
1.Extiende el lenguaje FAE para soportar las operaciones:
+ - * / < <= >= > == !=
Escribe pruebas para la extensión. (1pt)
|#
;hecho en clases



(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
(test (run '{+ 1 2 3 4}) 10)

(test (run '{with {x 3} 2}) 2)
(test (run '{with {x 3} x}) 3)
(test (run '{with {x 3} {with {y 4} x}}) 3)
(test (run '{with {x 3} {+ x 4}}) 7)
(test (run '{with {x 3} {+ x x x x}}) 12)
(test (run '{with {x 3} {with {x 10} {+ x x}}}) 20)
(test (run '{with {x 3} {with {x x} {+ x x}}}) 6)
(test (run '{with {x 3} {with {y 2} {+ x y}}}) 5)
(test (run '{with {x 3} {+ 1 {with {y 2} {+ x y}}}}) 6)
(test (run '{with {x 3} {with {y {+ 2 x}} {+ x y}}}) 8)
(test (run '{with {x 3} {if-tf {+ x 1} {+ x 3} {+ x 9}}}) 6)


; Adaptando las pruebas previas
;(test/exn (run '{f 10}) "undefined function") - el error partia de fundef-lookup
(test/exn (run '{f 10}) "undefined")

;(test (run '{f 10} (list '{define {f x} {+ x x}})) 20)
; 1. Asociar la funcion a un identificador
(test (run '{with {f {fun {x} {+ x x}}}{f 10}}) 20)
; 2. Usar la funcion directamente, como un lambda
(test (run '{{fun {x} {+ x x}} 10}) 20)

;(test (run '{add1 {add1 {add1 10}}}(list '{define {add1 x} {+ x 1}})) 13)
(test (run '{with {add1 {fun {x} {+ x 1}}}{add1 {add1 {add1 10}}}}) 13)



(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {x} {+ {add1 x} {add1 x}}}}
                        {foo 10}}}) 22)

(test (run '{with {add1 {fun {x} {+ x 1}}}
                  {with {foo {fun {f} {+ {f 10} {f 10}}}}
                        {foo add1}}}) 22)


; Pruebas para casos basicos
(test (run '{{fun {x}{+ x 1}} {+ 2 3}}) 6)
(test (run '{with {apply10 {fun {f} {f 10}}}
                  {with {add1 {fun {x} {+ x 1}}}
                        {apply10 add1}}}) 11)



#|
2.Extiende el lenguaje FAE con las operaciones lógicas and y or.
Además, agrega soporte para strings y concatenación de cadenas.
Escribe pruebas (1pt)
|#


; Logical AND tests
(test (run '{and #t #t}) #t)       ; True AND True should be True
(test (run '{and #t #f}) #f)       ; True AND False should be False
(test (run '{and #f #t}) #f)       ; False AND True should be False
(test (run '{and #f #f}) #f)       ; False AND False should be False

; Logical OR tests
(test (run '{or #t #t}) #t)        ; True OR True should be True
(test (run '{or #t #f}) #t)        ; True OR False should be True
(test (run '{or #f #t}) #t)        ; False OR True should be True
(test (run '{or #f #f}) #f)        ; False OR False should be False

; String operations test
(test (run "hello" ) "hello")                ; String literal test
(test (run '(concat "hello " "world") ) "hello world")






#|
3.Constant Folding
Esta es una optimización utilizada en compiladores para eliminar el cálculo innecesario de expresiones constantes y reemplazarlas directamente con su valor.
En general, si existe una operación que no contiene variables, se espera simplificarla.
 as siguientes pruebas deben pasar (2pt): 
|#
;hecho en clases





; expr -> expr
; elimina el cálculo innecesario de expresiones constantes y reemplazandolas directamente con su valor.

(define (constant-folding expr)
  (match expr
    [(num n) expr] ; Los números son constantes y no se pueden simplificar más
    [(bool b) expr] ; Lo mismo para los booleanos
    [(id x) expr] ; Las variables no se pueden simplificar
    [(str s) expr]
    [(prim op args) ; Para operaciones primitivas
     (let ([args (map constant-folding args)]) ; Simplificar recursivamente los argumentos
       (if (andmap (λ (x) (match x [(num _) #t] [_ #f])) args) ; Si todos los argumentos son constantes
           (num (apply (cdr (assq op primitives)) (map (λ (x) (match x [(num n) n])) args))) ; Realizar la operación y crear un nuevo nodo numérico
           (prim op args)))] ; Si no todos los argumentos son constantes, mantener la estructura original
    
    [(if-tf c et ef) ; Para expresiones condicionales
     (let ([c (constant-folding c)] [et (constant-folding et)] [ef (constant-folding ef)]) ; Simplificar recursivamente las partes
       (if (and (match c [(num n) #t] [_ #f]) ; Si la condición es constante
               (match et [(num n) #t] [_ #f]) ; y el caso verdadero es constante
               (match ef [(num n) #t] [_ #f])) ; y el caso falso es constante
           (if (num? c) et ef) ; Si la condición es verdadera, devolver el caso verdadero, si no, devolver el caso falso
           (if-tf c et ef)))] ; Si alguna parte no es constante, mantener la estructura original
    
    [(with x e b) ; Para expresiones con let
     (let ([e (constant-folding e)] [b (constant-folding b)]) ; Simplificar recursivamente las partes
       (if (num? e) ; Si la expresión vinculada es constante
           (subst b x e) ; Sustituir todas las ocurrencias de x en b con el valor de e
           (with x e b)))] ; Si no, mantener la estructura original
    
    [(fun arg body) ; Para funciones
     (let ([body (constant-folding body)]) ; Simplificar recursivamente el cuerpo de la función
       (fun arg body))] ; Mantener la estructura original para las funciones
    ))

; Función auxiliar para sustituir variables
(define (subst expr var val)
  (match expr
    [(num n) expr]
    [(bool b) expr]
    [(str s) expr]
    [(id x) (if (eq? x var) val expr)]
    [(prim op args) (prim op (map (λ (x) (subst x var val)) args))]
    [(if-tf c et ef) (if-tf (subst c var val) (subst et var val) (subst ef var val))]
    [(with x e b) (if (eq? x var) expr (with x (subst e var val) (subst b var val)))]
    [(fun arg body) (if (eq? arg var) expr (fun arg (subst body var val)))]))

(test (constant-folding (parse '{with {x {+ 1 2 {* 3 4}}} {+ 1 x}}))
      (parse '{with {x 15} {+ 1 x}}))
(test (constant-folding (parse '{with {x {+ y 2 {* 3 4}}} {+ 1 x}}))
      (parse '{with {x {+ y 2 12}} {+ 1 x}}))
(test (constant-folding (parse '{if-tf {< x 1} {+ 3 3} {+ 5 9}}))
      (parse '{if-tf {< x 1} 6 14}))
(test (constant-folding (parse'{{fun {x} {+ x {* 2 4}}} {+ 5 5}}))
      (parse '{{fun {x} {+ x 8}} 10}))



#|


4. Constant Propagation.
 Otra optimizacion en compiladores es constant propagation.Cuando se reconoce que un identificador es constante,
entonces se reemplazan sus ocurrencias inmediatamente en lugar de mantener la substitución hasta el final.
Las siguientes pruebas deben pasar (2pt): 


|#


;solucion discutida con christian rivero


(define (constant-propagation-env-lookup x env)
  (match env
    [(mtEnv) (id x)]
    [(aEnv id val tail)(if (eq? id x) val (constant-propagation-env-lookup x tail))]
    )
  )
;constant-propagation   (expr,env) ->  expr
;reemplazan sus ocurrencias de ids inmediatamente en lugar de mantener la substitución hasta el final
(define (constant-propagation expr [env (mtEnv)])
  (match expr
    [(num n) (num n)]
    [(bool b) (bool b)]
    [(id x) (constant-propagation-env-lookup x env)]; buscar el valor de x en env
    [(prim prim-name args) (prim prim-name (map  (λ (arg) (constant-propagation arg env))  args))]
    [(if-tf c et ef) (if-tf (constant-propagation c env)
                         (constant-propagation et env)
                         (constant-propagation ef env))]
    [(with x e b) (with x e (constant-propagation b (extend-env x e env)))] ; Si asociamos una funcion a una variable, la funcion entra al env
    [(fun arg body) (closureV arg body env)] 
    [(app f e)
     (app f (constant-propagation e env))
     ]
)
  )

(test (constant-propagation
   (parse '{with {x 3} {+ x x}})) (parse '{with {x 3} {+ 3 3}}))
(test (constant-propagation
   (parse '{with {x 3} {with {y 5} {+ x y}}})) (parse '{with {x 3} {with {y 5} {+ 3 5}}}))
(test (constant-propagation
   (parse '{with {x 3} {with {y 5} {+ z z}}})) (parse '{with {x 3} {with {y 5} {+ z z}}}))





#|
5. Dead-Store Elimination. Esta es otra optimización que quita las variables que no tienen
un uso al haber sido declaradas. Es decir, si la variable dentro de un with no es usada en el cuerpo, se puede simplificar la expresión. Las siguientes pruebas deben pasar (2pt):

|#



(define (is-var-used-in-body var-id expr)
    (match expr
    [(id x) (if (eq? var-id x) #t #f)]
    [(num n) #f]
    [(bool b) #f]
    [(prim prim-name args)   (member #t (map  (λ (arg) (is-var-used-in-body var-id arg) )   args))]
    [(if-tf c et ef) (or (is-var-used-in-body var-id c)
                         (is-var-used-in-body var-id et)
                         (is-var-used-in-body var-id ef)
                         )
                     ]
    [(with x e b) (or
                   (is-var-used-in-body var-id e)
                   (is-var-used-in-body var-id b )
                   )
                 ]
    [(fun arg body) (if (eq? arg var-id) #f (is-var-used-in-body var-id body))] 
    [(app f e)
     (or (is-var-used-in-body var-id f)
         (is-var-used-in-body var-id e)
       )
     ]
    )
  )

;dead-store-elimination expr->expr
;quita las variables que no tienen un uso al haber sido declarada
(define (dead-store-elimination expr )
  (match expr
    [(num n) (num n)]
    [(bool b) (bool b)]
    [(id x) (id x)]
    [(prim prim-name args) (prim prim-name (map  (λ (arg) (dead-store-elimination arg))  args))]
    [(if-tf c et ef) (if-tf (dead-store-elimination c )
                         (dead-store-elimination et )
                         (dead-store-elimination ef ))]
    [(with x e b) (if (is-var-used-in-body x b) expr b)] 
    [(fun arg body) (if (is-var-used-in-body arg body) expr body)]
    [(app f e)
     (app (dead-store-elimination f) (dead-store-elimination e)) 
     ]
)
  )
(test (dead-store-elimination (parse '{with {x 3} {+ 1 2 3 4}})) (parse '{+ 1 2 3 4}))
(test (dead-store-elimination (parse '{fun {x} {+ 1 2 3 4}})) (parse '{+ 1 2 3 4}))
(test (dead-store-elimination (parse '{fun {x} {+ 1 2 x 4}})) (parse '{fun {x} {+ 1 2 x 4}}))




#|
6. Dynamic Keyword. Permite que una función sea evaluado con scope dinámico en lugar de estático usando el keyword dynamic para permitir la aplicación de función con scope dinámico. Usa el siguiente ejemplo/prueba guía (2pt): 

El {dynamic f 2} utiliza como valor de n 2. Mientras tanto, {f 2} mantiene el valor de n en 1. 
; este  me lo tuv que explicar chris ya que yo solo originalmente queria modioficar el app original para que soporte ambo tipos de scope , de alguna forma modificando el interp , pero obviaente eso no funciono
|#

(test (run '{with {n 1}
                  {with {f {fun {z} {+ n z}}}
                        {with {n 2}
                              {+ {dynamic f 2} {f 2}}}}}) 7)




