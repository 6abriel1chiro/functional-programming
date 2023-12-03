#lang play


; apuntes mwed 22 nov
;

#|
<FAE> ::=   <num> | <bool> | <id>
            | (+ <FAE> <FAE>)
            | (- <FAE> <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento. 
            | (fun <id> <FAE>) ; fun(que es una lambda) nombre-arg body
            | (refun <id> <FAE>) ; call by reference
            ; cajas mutables
            | (newbox <expr>)
            | (setbox <expr> <expr>)
            | (openbox <expr>)
            | (seqn <expr> <expr>) ; esta es una secuencia el ; en C o begin en Scheme
            | set <id> <expr> 

|#

; Ejemplo de box
#|
{with {b {newbox 10}}
  {seqn
       {setbox b 20}
       {openbox b}}
}

{with {make-box {fun {x} {newbox x}}}
      {setbox {make-box 10} 20}
}

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
  [refun arg body]      ; (refun <id> <FAE>) ;
  [newbox b] ; newbox 4
  [openbox b]
  [setbox b n] ; [setbox (newbox 4) 5]
  [seqn e1 e2]
  [set id e] ; [set id expr]
  
)

; forma mas directa de aceptar n argumentos : concatenar (parser por debajo)


;1 actualizar environment

#|
<env> ::= (mtEnv)
          | (aEnv <id> <loc> <env>)
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
    [(aEnv id loc tail)(if (eq? id x) loc (env-lookup x tail))]
    )
  )

; paso 2 : crear sto (storage)

(deftype sto
  [mtSto]
  [aSto loc val next-sto]
  )

;empty-sto
;extend-sto
;sto-lookup


; empty-env -> (mtEnv)
(define empty-sto (mtSto))

; extend-env:: <loc> <val> sto -> <env>
(define extend-sto aSto)

; sto-lookup :: <loc> <sto> -> <val>
; buscar el valor de una variable dentro del storage
(define (sto-lookup l sto)
  (match sto
    [(mtSto) (error "segmantatio fault: " l)]
    [(aSto loc val tail)(if (eq? loc l) val (sto-lookup l tail))]
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
    [(list 'set id e) (set id (parse e))]
    [(list arg e) (app (parse arg) (parse e))]
    [(list 'fun (list arg) body) (fun arg (parse body))]
    [(list 'refun (list arg) body) (refun arg (parse body))]
    )
  )


;4. incluir un nuevo tipo de valor v*s

(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env
  (v*s val sto) ; 3. soporte para val-sto par
  (boxV loc)
  (voidV)
  (refclosV arg body env) ;referencial closure value
  )



;3. actualizar interp
; interp :: Expr  Env sto -> [val*sto]
; interpreta una expresion
(define (interp expr env sto)
  (match expr
    [(num n) (v*s (valV n) sto)]
    [(bool b) (v*s (valV b) sto)]
    [(fun arg body) (v*s (closureV arg body env) sto)]
    
    [(refun arg body) (v*s (refclosV arg body env) sto)]

    [(id x) (v*s (sto-lookup (env-lookup x env) sto) sto)] ;6 Actualizar busqueda en sto
    
    [(add l r)
     ; 1. evaluar l
     (def (v*s l-val l-sto) (interp l env sto))
     ;2 evaluar r
     (def (v*s r-val r-sto) (interp r env l-sto))
     ; 3. efectuar suma. devolver par
     (v*s (valV+ l-val r-val) r-sto)
     ]
   
    [(sub l r)
     ; 1. evaluar l
     (def (v*s l-val l-sto) (interp l env sto))
     ;2 evaluar r
     (def (v*s r-val r-sto) (interp r env l-sto))
     ; 3. efectuar resta. devolver par
     (v*s (valV- l-val r-val) r-sto)]

    
    
    [(if-tf c et ef)
     
    (def (v*s c-val c-sto) (interp c env sto))
     
    (if (valV-v c-val)
     (interp et env c-sto)
     (interp ef env c-sto))]

     [(app f e)
      ;0 interp f
     (def (v*s fun-val fun-sto) (interp f env sto))
     (match fun-val
        [(closureV arg body fenv)
     ; 1 interp e
     (def (v*s arg-val arg-sto) (interp e env fun-sto))
     ;2  obt nueva direccion de memoria
     (def new-loc (malloc arg-sto))
     ;3 interp body
     (interp body
             (extend-env arg new-loc fenv)
             (extend-sto   new-loc arg-val arg-sto ))] ;call by val
       [(refclosV arg body fenv) (v*s voidV sto)
          ;1 l-val e (id 'v)
          (define loc (env-lookup (id-name e) env))
          ;2  interp body en fenv (el env al momento de definir funcion)
          (interp body (extend-env arg loc fenv) fun-sto)
          
          ] ;call by ref
       )
     ]


    
    [(newbox b)
     ;1. crear direccion de memoria
     ;(def new-loc (malloc sto))
     ;2. interp b
     (def (v*s b-val b-sto) (interp b env sto))
     (def new-loc (malloc b-sto))
     ;3 retornar con store actualizado
     (v*s (boxV new-loc) (extend-sto new-loc b-val b-sto))
     ]

    [(openbox b)
     ;1. interp b
     (def (v*s (boxV l) b-sto) (interp b env sto))
     ;2  sto-lookup boxV
      (v*s  (sto-lookup l b-sto) b-sto)

     ]


      [(setbox b n)
     ;1. interp b
      (def (v*s (boxV loc) b-sto) (interp b env sto))
     ;2  interp n
      (def (v*s n-val n-sto) (interp n env b-sto))
      ;3. actualizar sto
      (v*s (boxV loc) (extend-sto loc n-val n-sto))
     ]

[(seqn e1 e2)
     ;1. interp e1
     (def (v*s e1-val e1-sto) (interp e1 env sto))
     ;2. interp e2 (con lo ultimo)
     (interp e2 env e1-sto)
     ;3 return e2
    ; (v*s e2-val e2-sto)
     
     ]
    [(set id e)
     ; 1 interp e
      (def (v*s e-val e-sto) (interp e env sto))
     ; 2 return void (extend sto . . loc.   e-val e-sto )
       (v*s (voidV) (extend-sto (env-lookup id env) e-val e-sto))
     
     ]
))

(define (malloc sto)
  (match sto
    [(mtSto) 0]
    [(aSto loc _ tail) (+ 1 (malloc tail))]
    )
  ) ; malardo


; valV+ : Val -> Val
(define (valV+ s1 s2)
  (valV (+ (valV-v s1) (valV-v s2)))
  )

(define (valV- s1 s2)
  (valV (- (valV-v s1) (valV-v s2)))
  )





; run: Src -> Src
; corre un programa
(define (run prog)
  (def (v*s res sto) (interp (parse prog) empty-env empty-sto))
    (match res
      [(valV v) v]
      [(closureV arg body env) res]
      [(boxV loc) res]

      )
    )
  

(test (run '{+ 3 4}) 7)
(test (run '{- 5 1}) 4)
 (run '{with {x 3} 2}) 
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



(run '(newbox (+ 1 2)))

(test (run '{with {b {newbox 10}}
                  {seqn
                   {setbox b 20}
                   {openbox b}}}) 20)



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



(run '{with {a 10}
            {with {f {fun {x} {+ x a}}} ; valor de a?
                  {seqn
                   {set a 20}
                   {with {a 3}
                         {f 2}}}}})


(run '{with {v 0}
            {with {f {refun {x} {set x 5}}}
                  {seqn {f v}
                        v}}})






; INTRODUCTION TO MACROS
(define v 5)


(run '{with {swap {refun {a}
                       {refun {b}
                            {with {tmp a}
                                  {seqn
                                   {set a b}
                                   {set b tmp}}}}}}
            {with {a 10}
                  {with {b 20}
                  {seqn {{swap a} b}
                        b}}
            }})





(let ((v 0))
  (let ((f (lambda (x) (set! x 5))))
    (begin
      (f v)
      v)))



(define (swap a b)
  (let ([tmp a])
    (set! a b)
    (set! b tmp)
    )
  )

(let ([x 10] [y 20])
  (begin
    (swap x y )
    y
    )
  )



(let ([x 10] [y 20])
(begin
(let ([tmp x])
(set! x y)
(set! y tmp)
y)))





(defmac (swap-m a b)
  (let ([temp a])
    (set! a b)
    (set! b temp)))




(let ([tmp 10] [tmp2 20])
  (begin
    (swap-m tmp tmp2)
    (- tmp tmp2)
    ))




#|
parse : src → expr
interp : expr → val
analyze : expr → info / type / ext
optimize : expr → expr
transform : expr → expr
compile : expr(L1) → expr(L2) / IR(L2)
exec : IR → val



(let ([tmp 10] [tmp2 20])
  (begin
    (swap-m tmp tmp2)
    (- tmp tmp2)
    ))

macros higienicas

|#


(define (obj x) 
(λ () x))
;mobject --> objs
  ;field



(define counter
  (let ([count 0])
    (λ (msg)
      (match msg
        ['inc (begin
        (set! count (add1 count))
        count)]
['dec (begin
        (set! count (- count 1))
        count)]
        ['reset (begin
        (set! count (0))
        count)]
        ))))


(define counter2
  (let ([count 0]
        [step 1])
    (λ (msg . args) ; recibe 1 o mas
      (match msg
        ['inc (begin ;1
        (set! count (+ count step))
        count)]
        ['dec (begin ;2
        (set! count (- count step))
        count)]
        ['reset (begin ;3
        (set! count (0))
        count)]
        ['step!  ;4
        (set! step (first args))]
        )
      )
    )
  )




(counter2 'inc)
(counter2 'dec)
(counter2 'step! 2)
(counter2 'dec)





(define counter3
  (let ((count 0)
        (step 1))
    (let ((methods (list
                     (cons 'inc (lambda ()
                                   (set! count (+ count step))
                                   count))
                     (cons 'dec (lambda ()
                                   (set! count (- count step))
                                   count))
                     (cons 'reset (lambda ()
                                   (set! count 0)
                                   count))
                     (cons 'step! (lambda (args)
                                   (set! step args )
                                   ) ))))
        (lambda (msg . args)
          (let ((found (assoc msg methods)))
            (if found
              (apply (cdr found) args)
              (error "Not understood method: " msg)))))
    ))



(counter3 'inc)
(counter3 'dec)
(counter3 'step! 2) 
(counter3 'dec)
(counter3 'reset)
;define a macro that has the form of couner3 and extends counter4

(defmac (OBJECT
        ([field fname fval] ...)  ; 0 o mas
        ([method mname mparams mbody  ... ] ... )) ; 0 o mas
        #:keywords field method
          (let ([fname fval] ...)
            
         (let ([methods (list
                     (cons 'mname (lambda mparams mbody ...)) ... )])
           
        (lambda (msg . args)
          (let ((found (assoc msg methods)))
            (if found
              (apply (cdr found) args)
              (error "Not understood method: " msg)
              )
            )
          )
       )
     )
  )



  

(define (counter4 )
  (OBJECT
   ;FIELDS
   ([field count 0]
    [field step 1])
   ;methods
   (
    [method inc () (set! count (+ count step)) count]
    [method dec () (set! count (- count step)) count]
    [method reset () (set! count 0)]
    [method step! (v) (set! step v)]
    )
   )
  )




 ; create a new object  with 3  fields and 2 metodos where one of them has 3 parameters


(defmac (→ o m arg ... )
(o 'm arg ...)
)




; fabricas de objetos


(define (addn  n)
  (λ (m)
  (+ n m)))
















(define (make-count (counter 0) (stepper 1))
  (OBJECT
    ;FIELDS
    ([field count counter]
     [field step stepper])
    ;METHODS
    (
      [method inc () (set! count (+ count step)) count]
      [method dec () (set! count (- count step)) count]
      [method reset () (set! count 0)]
      [method step! (v) (set! step v)]
    )
  )
)


(let ([c1 (make-count )]  [c2 (make-count 6 1)])
   (+ (→ c1 inc) (→ c2 inc))
  )

(define (all-inc lst )
  (map (λ (o) (→ o inc )) lst))

(all-inc (list
          (make-count)
          (make-count 2 3)
          (OBJECT () ([method inc () "hello" ]))))









(define counter5
  ;fields
  (letrec ([ self (let ([count 0]
        [step 1])
    (let ([methods (list
                     (cons 'inc (lambda ()
                                   (set! count (+ count step))
                                   count))
                     (cons 'dec (lambda ()
                                   (set! count (- count step))
                                   count))
                     (cons 'reset (lambda ()
                                   (set! count 0)
                                   count))
                     (cons 'step! (lambda (args)
                                   (set! step args )
                                   ) )
                     (cons 'inc-by! (λ (v) (self 'step! v) (self 'inc)))
                     )])
        (lambda (msg . args)
          (let ((found (assoc msg methods)))
            (if found
              (apply (cdr found) args)
              (error "Not understood method: " msg)
      )
    )
    )
      )))











