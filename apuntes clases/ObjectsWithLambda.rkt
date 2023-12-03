#lang play
(define counter
  (let ([count 0])
    (λ () (begin (set! count (add1 count)) count)))
  )

; Object -> counter
; Fields -> count
; Methods -=> inc()

; dec reset
(define counter1
  (let ([count 0])
    (λ (msg)
      (match msg ; dispatch
        ['inc (begin (set! count (add1 count)) count)]
        ['dec (begin (set! count (sub1 count)) count)]
        ['reset (set! count 0)]
        )
      )
  ))

(counter1 'inc)
(counter1 'inc)
(counter1 'dec)
(counter1 'reset)
(counter1 'dec)

(define counter2
  (let ([count 0]
        [step 1])
    (λ (msg . args) ; recibe 1 o mas
      (match msg ; dispatch
        ['inc (begin (set! count (+ count step)) count)]
        ['dec (begin (set! count (- count step)) count)]
        ['reset (set! count 0)]
        ['step! (set! step (first args))]
        )
      )
  ))


(counter2 'inc); --> 1
(counter2 'inc); --> 2
(counter2 'step! 10) 
(counter2 'dec); --> -8
(counter2 'reset) 
(counter2 'dec) ; --> -10



(define counter3
  ; Fields
  (let ([count 0]
        [step 1])
    ; Methods
    (let ([methods (list
                    (cons 'inc (λ () (set! count (+ count step)) count))
                    (cons 'dec (λ () (set! count (- count step)) count))
                    (cons 'reset (λ () (set! count 0)))
                    (cons 'step! (λ (v) (set! step v)))
                    )])
      (λ (msg . args)
        (let ([found (assoc msg methods)])
          (if found
              (apply (cdr found) args)
              (error "Not understood method: " msg)
          )
        )
      )
  )))

(defmac (OBJECT
         ([field fname fval] ...) ; 0 o mas
         ([method mname mparams mbody ...] ...))
         #:keywords field method
         (let ([fname fval] ...)
           (let ([methods (list
                    (cons 'mname (λ mparams mbody ...)) ...
                    )])
             (λ (msg . args)
               (let ([found (assoc msg methods)])
                 (if found
                     (apply (cdr found) args)
                     (error "Not understood method: " msg)
                     )
                 )
               )
             )
  ))


(define counter4
  (OBJECT
   ; Fields
   ([field count 0]
    [field step 1])
   ; Methods
   ([method inc () (set! count (+ count step)) count]
    [method dec () (set! count (- count step)) count]
    [method reset () (set! count 0)]
    [method step! (v) (set! step v)]
    )
   )
  )


(counter4 'inc) ; --> 1
(counter4 'step! 13)
(counter4 'inc) ; --> 14
(counter4 'inc) ; --> 27
(counter4 'inc) ; --> 40
(counter4 'step! 10)
(counter4 'dec) ; --> 30
(counter4 'reset) 
(counter4 'dec) ; --> -10


(define my-point
  (OBJECT
   ([field x 0]
    [field y 0]
    )
   (
    [method setX (x-new) (set! x x-new)]
    [method setY (y-new) (set! y y-new)]
    [method getLocation () (cons x y)]
    [method toString () (def x-str (number->string x))
                         (def y-str (number->string y))
                         (string-append "x:" x-str "  " "y:" y-str)]
    )
   ))


(my-point 'getLocation)
(my-point 'setX 9)
(my-point 'setY 6)
(my-point 'getLocation)
(my-point 'toString)
