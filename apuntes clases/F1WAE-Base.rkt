#lang play
#|
Bien, ahora nuestro lenguaje WAE soporta variables, pero las
variables no son suficientes para poder ejecutar programas, el
paso logico que sigue son las funciones.

Existen distintos tipos de funciones:
1. First-order functions, funciones que no son valores, es decir
son solo identificadores relacionados a una expresion. A este
nivel no se pueden enviar como argumentos o como retorno.
<fun> ::= <id> <expr>

2. Higher-order functions, funciones que pueden retornar otras
funciones.

3. First-class functions, funciones que tienen todos los
derechos de un valor. Pueden ser argumentos, valores de retorno,
etc. A este nivel, es posible tener map, filter, fold, etc. Otra
tecnica de este nivel es la currificacion.
<fun> ::= <expr> <expr>

Nuestro lenguaje poco a poco soportara funciones como valores,
pero por ahora, empecemos con lo basico, soportar first-order
functions por lo que el lenguaje pasara a ser llamado F1WAE.

Para eso, consideremos dos aspectos:

1. Para definir funciones de primer orden, no es necesario que
las funciones sean un nuevo tipo de valor. Es decir, las
definiciones de funcion NO SON parte de <expr>

Dado que no son parte de <expr>, se deben definir de otra forma,
las definiremos en una lista separada como un tipo fundef:

fundef fname arg-name  body
        foo     x     (+ x x)

Y nuestro programa correra como:
<prog> ::= <expr> <fundefs>
           main    funciones

Similar a tener un main y una lista de funciones de referencia
de las cuales puede obtener el cuerpo.

2. La aplicacion de una funcion contempla solamente
identificadores en posicion de funcion.

(foo 5)
  |_ es un <id> no un <expr>

Lo nuevo que si va a soportar el lenguaje es un nodo de aplicar
funcion

Sintaxis Concreta: '{foo {+ 2 3}}
Sintaxis Abstracta: (app 'foo (add (num 2) (num 3)))
                          fname        arg
Que es distinto de la definicion de la funcion.
Entonces, los pasos a seguir son:

    1. Crear la estructura para definir funciones
    2. Extender gramatica con la aplicacion de funcion
    3. Modificar parser
    4. Actualizar interp con la lista de funciones
    5. Actualizar run
    6. Probar
|#


#|
<F1WAE> ::=   <num> | <bool> | <id>
            | (+ <F1WAE> <F1WAE>)
            | (- <F1WAE> <F1WAE>)
            | (if-tf <F1WAE> <F1WAE> <F1WAE>)
            | (with <id> <F1WAE> <F1WAE>)
            | (app <id> <F1WAE>) ; aplicacion de funcion
|#

(deftype Expr
  [num n]                                 ; <num>
  [bool b]                                ; <bool>
  [add l r]                               ; (+ <F1WAE> <F1WAE>)
  [sub l r]                               ; (- <F1WAE> <F1WAE>)
  [if-tf c et ef]                         ; (if-tf <F1WAE> <F1WAE> <F1WAE>)
  [with id-name named-expr body-expr]     ; (with <id> <F1WAE> <F1WAE>)
  [id name]                               ; <id> 
  [app fname arg-expr]                    ; (app <id> <F1WAE>)
)

; <fundef> := {define {<id> <id>} <expr>}
(deftype Fundef
  (fundef fname arg body)
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
    [(list 'with (list x e) b) (with x (parse e) (parse b))]
    [(list fname e) (app fname (parse e))]
    [(list 'define (list name arg) body)(fundef name arg (parse body))]
    )
  )

#|
; fun-parse: Src -> Expr
; parsea funciones, esto esta ahora por separado, pero puede integrarse al parser definido
(define (fun-parse src)
  (match src
    [(list 'define (list name arg) body)(fundef name arg (fun-parse body))]
    [else (parse src)]
    )
  )
|#


;subst :: id expr expr -> expr
;substituir x por v en e
(define (subst x v e)
  (match e
    [(num n) e]
    [(bool b) e]
    [(id sym) (if (eq? x sym) v e)]
    [(add l r) (add (subst x v l) (subst x v r))]
    [(sub l r) (sub (subst x v l) (subst x v r))]
    [(if-tf c et ef) (if-tf (subst x v c) (subst x v et) (subst x v ef))]
    [(with id ne b)
     (with id (subst x v ne)
           (if (eq? x id)
               b
               (subst x v b)
               ))]
   )
 )

; lookup-fundef: id fundefs -> fundef o error
(define (lookup-fundef fname fundefs)
  (match fundefs
    [(list) (error "undefined function: " fname)]
    [(cons fd fds) (if (eq? (fundef-fname fd) fname)
                       fd
                       (lookup-fundef fname fds)
                       )]
    )
  )

; interp :: Expr List(fundefs) -> val
; interpreta una expresion
(define (interp expr fundefs)
  (match expr
    [(num n) n]
    [(bool b) b]
    [(id x) (error "undefined: " x)]
    [(add l r) (+ (interp l fundefs) (interp r fundefs))]
    [(sub l r) (- (interp l fundefs) (interp r fundefs))]
    [(if-tf c et ef) (if (interp c fundefs)
                         (interp et fundefs)
                         (interp ef fundefs))]
    [(with x e b) ; {with {x e} b}
     (interp (subst x (parse (interp e fundefs)) b) fundefs)]
    [(app f e)
     ; 1. buscar la funcion
     (def (fundef name arg body) (lookup-fundef f fundefs))
     ; 2. evaluar el argumento
     ; 3. substituir arg-val por x en body
     ; 4. interp
     (interp (subst arg (parse (interp e fundefs)) body) fundefs)
     ]
))

; run: Src list<fundef>? -> Expr
; corre un programa
(define (run prog [fundefs '()])
  (interp (parse prog) (map parse fundefs))
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


(test/exn (run '{f 10}) "undefined function")
(test (run '{f 10} (list '{define {f x} {+ x x}})) 20)


