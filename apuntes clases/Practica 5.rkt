#lang play
(print-only-errors #f) ; Para ver solo los errores.
;Practica 5
;Nombre: Christian Rivero Arnez


;1.- Modifica el intérpreta para que evalúe la suma de derecha a izquierda y escribe pruebas que
;demuestren que el resultado cambia de acuerdo al orden de evaluación. (2pts)

#|
<FAE> ::=   <num> | <bool> | <id>
            | (+ <FAE> <FAE>)
            | (- <FAE> <FAE>)
            | (if-tf <FAE> <FAE> <FAE>)
            | (with <id> <FAE> <FAE>)
            | (app <FAE> <FAE>) ; puedo aplicar una funcion a otra funcion / puedo usar una funcion como argumento. 
            | (fun <id> <FAE>) ; fun(que es una lambda) nombre-arg body
            ; cajas mutables
            | (newbox <expr>)
            | (setbox <expr> <expr>)
            | (openbox <expr>)
            | (seqn <expr> <expr>) ; esta es una secuencia el ; en C o begin en Scheme
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
  [newbox b]
  [openbox b]
  [setbox b n]
  [seqn e1 e2]
) 


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

(deftype Sto
  (mtSto)
  (aSto loc val sto)
  )



(define empty-sto (mtSto))

(define extend-sto aSto)

(define (sto-lookup l sto)
  (match sto
    [(mtSto) (error "segmentation fault:" l)]
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
    [(list '+- s1 s2) (add (parse s2) (parse s1))]
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


(deftype Val
  (valV v) ; numero, booleano, string, byte, etc.
  (closureV arg body env) ; closure = fun + env
  (v*s val sto)
  (boxV loc)
  )

; interp :: Expr  Env, Sto ->  Val*sto 
; interpreta una expresion
(define (interp expr env sto)
  (match expr
    [(num n) (v*s (valV n) sto)]
    [(bool b) (v*s (valV b) sto)]
    [(fun arg body) (v*s (closureV arg body env) sto)]
    
    [(id x) (v*s (sto-lookup (env-lookup x env) sto) sto)]

    
    [(add l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (valV+ l-val r-val) r-sto)]
    
    [(sub l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (valV- l-val r-val) r-sto)]
    [(if-tf c et ef)
     (def (v*s cond-val cond-sto) (interp c env sto))
     (if (valV-v cond-val)
         (interp et env cond-sto) 
         (interp ef env cond-sto))]
    
    [(app f e)
     (def (v*s (closureV arg body fenv) fSto) (interp f env sto))
     (def (v*s exp-val exp-sto) (interp e env fSto))
     (def new-loc (malloc exp-sto))
     
     (interp body (extend-env arg new-loc fenv) (extend-sto new-loc exp-val exp-sto))]
    [(newbox b)
     
     (def (v*s exp-val exp-sto) (interp b env sto))
     (def new-loc (malloc exp-sto))
     (v*s (boxV new-loc) (extend-sto new-loc exp-val exp-sto))
     ]
    [(openbox b)
     (def (v*s (boxV loc) intSto) (interp b env sto))
     (v*s (sto-lookup loc intSto) intSto)
     ]
    [(setbox b n)
     (def (v*s (boxV loc) intSto) (interp b env sto))
     (def (v*s exp-val exp-sto) (interp n env intSto))
     
     (v*s (boxV loc) (extend-sto loc exp-val exp-sto))
     ]
    [(seqn e1 e2)
     (def (v*s e1val e1sto) (interp e1 env sto))
     (def ans (interp e2 env e1sto))
     ans
     ]

))

(define (malloc sto)
  (match sto
    [(mtSto) 0]
    [(aSto loc _ tail) (+ 1 (malloc tail))]
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
(define (run prog)
  (def (v*s res sto) (interp (parse prog) empty-env empty-sto)) 
    (match res
      [(valV v) v]
      [(closureV arg body env) res]
      [(boxV loc) res])
  
    )

; suma normal
(test (run '{+ 3 4}) 7)
; suma inversa
(test (run '{+- 3 4}) 7)

; con estas pruebas se ve que el orden importa ya que en la primera el store cambia al inicio y suma 10 +10
; en el otro caso suma 5+10
(test (run '(with {box1 (newbox 5)} {+ (seqn (setbox box1 10)(openbox box1)) (openbox box1)})) 20) ; normal
(test (run '(with {box1 (newbox 5)} {+- (seqn (setbox box1 10)(openbox box1)) (openbox box1)})) 15) ; inversa


;2.- Permite que seqn ya no acepte sólamente un par de instrucciones, sino que procese n instrucciones,
;escribe pruebas para esa ejecución. (4pts)


(define (parse2 src)
  (match src
    [(? number?) (num src)]
    [(? boolean?) (bool src)]
    [(? symbol?) (id src)]
    [(list '+ s1 s2) (add (parse2 s1) (parse2 s2))]
    [(list '+- s1 s2) (add (parse2 s2) (parse2 s1))]
    [(list '- s1 s2) (sub (parse2 s1) (parse2 s2))]
    [(list 'if-tf c et ef) (if-tf (parse2 c) (parse2 et) (parse2 ef))]
    [(list 'with (list x e) b) (app (fun x (parse2 b)) (parse2 e))]
    [(list 'newbox b) (newbox (parse2 b))]
    [(list 'openbox e) (openbox (parse2 e))]
    [(list 'setbox b v) (setbox (parse2 b) (parse2 v))]

    
    [(list 'seqn (cons e1 tail)) 
                                  (if (empty?  tail)  (parse2 e1)  (seqn (parse2 e1) (parse2 (list 'seqn tail ))))
                                 ]

    [(list arg e) (app (parse2 arg) (parse2 e))]
    [(list 'fun (list arg) body) (fun arg (parse2 body))]
    ) 
  )
 

(define (run2 prog)
  (def (v*s res sto) (interp (parse2 prog) empty-env empty-sto)) 
    (match res
      [(valV v) v]
      [(closureV arg body env) res]
      [(boxV loc) res])
  
    )


(test (run2 '(with {box1 (newbox 5)} {+ (seqn {(setbox box1 10) (setbox box1 15) (setbox box1 35) (openbox box1)}) (openbox box1)})) 70)
(test (run2 '(with {box1 (newbox 5)} {+ (seqn {(setbox box1 10)(setbox box1 55)(+ (openbox box1) 10)}) (openbox box1)})) 120)

;3.- Por ahora, cada cambio en sto implica agregar un nuevo registro encima de todos los anteriores.
;Modifica la implementación para que, sin usar mutación, se elimine el último registro del sto antes de actualizarlo. (4pts)


(define (sto-cleanup l sto)
  (match sto
    [(mtSto) (mtSto)]
    [(aSto loc val tail)(if (eq? loc l) tail (aSto loc val (sto-cleanup l tail)) )]
    )
  
  )
(define (printSto sto)
  (match sto
    [(mtSto) (mtSto)]
    [(aSto loc val tail) (begin
                           (display loc)
                           (display val)
                           (display "\n")
                           (printSto tail))]
    )
  )

(define (interp2 expr env sto)
  (match expr
    [(num n) (v*s (valV n) sto)]
    [(bool b) (v*s (valV b) sto)]
    [(fun arg body) (v*s (closureV arg body env) sto)]
    [(id x) (v*s (sto-lookup (env-lookup x env) sto) sto)]
    [(add l r)
     (def (v*s l-val l-sto) (interp2 l env sto))
     (def (v*s r-val r-sto) (interp2 r env l-sto))
     (v*s (valV+ l-val r-val) r-sto)]
    [(sub l r)
     (def (v*s l-val l-sto) (interp2 l env sto))
     (def (v*s r-val r-sto) (interp2 r env l-sto))
     (v*s (valV- l-val r-val) r-sto)]
    [(if-tf c et ef)
     (def (v*s cond-val cond-sto) (interp2 c env sto))
     (if (valV-v cond-val)
         (interp2 et env cond-sto) 
         (interp2 ef env cond-sto))]
    
    [(app f e)
     (def (v*s (closureV arg body fenv) fSto) (interp2 f env sto))
     (def (v*s exp-val exp-sto) (interp2 e env fSto))
     (def new-loc (malloc exp-sto))
     (interp2 body (extend-env arg new-loc fenv) (extend-sto new-loc exp-val exp-sto))]
    [(newbox b)
     
     (def (v*s exp-val exp-sto) (interp2 b env sto))
     (def new-loc (malloc exp-sto))
     (v*s (boxV new-loc) (extend-sto new-loc exp-val exp-sto))
     ]
    [(openbox b)
     (def (v*s (boxV loc) intSto) (interp2 b env sto))
     (v*s (sto-lookup loc intSto) intSto)
     ]
    [(setbox b n)
       (def (v*s (boxV loc) intSto) (interp2 b env sto))
       (def (v*s exp-val exp-sto) (interp2 n env intSto))
       (def new-sto (sto-cleanup loc exp-sto))
     (begin
       (display "pre cleanupSto\n")
       (printSto exp-sto)
       (display "post cleanupSto of loc: ")
       (display loc)
       (display "\n")
       (printSto new-sto)
       (v*s (boxV loc) (extend-sto loc exp-val new-sto))
      )
     ]
    [(seqn e1 e2)
     (def (v*s e1val e1sto) (interp2 e1 env sto))
     (def ans (interp2 e2 env e1sto))
     ans
     ]

))
(define (run3 prog)
  (def (v*s res sto) (interp2 (parse prog) empty-env empty-sto)) 
    (match res
      [(valV v) v]
      [(closureV arg body env) res]
      [(boxV loc) res])
  
    )

(run3 '(with {box1 (newbox 5)} (seqn (setbox box1 44) (openbox box1))))
(run3 '(with {x 30} (with {box1 (newbox x)} (seqn (setbox box1 75) (with (z 3) (+ z (openbox box1)))))))

