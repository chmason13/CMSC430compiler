#lang racket

(provide desugar)

(require "utils.rkt")



(define (prefix x)
  (if (equal? "$" (substring (symbol->string x) 0 1))
      x
      (string->symbol (string-append "$" (symbol->string x)))))

(define (ext-env h xs0 xs1)
  (foldl (lambda (x y h) (hash-set h x y)) h xs0 xs1))

(define (t-letrec* e env)
  (match e
         [`(,xletrec* ([,xs ,es] ...) ,e0)
          (t-desugar
            `(%%let ,(map list xs (map (lambda (x) ''()) xs))
               (%%begin
                 ,@(map (lambda (x e) `(%%set! ,x ,e)) xs es)
                 ,e0))
            env)]))

(define (t-letrec e env)
  (match e  
         [`(,xletrec ([,xs ,es] ...) ,e0)
          (define ts (map (lambda (x) (gensym 'letrec)) xs))
          (t-desugar
            `(%%let ,(map list xs (map (lambda (x) ''()) xs))
               (%%let ,(map list ts es)
                 (%%begin
                   ,@(map (lambda (x t) `(%%set! ,x ,t)) xs ts)
                   ,e0)))
            env)]))

(define (t-let* e env)
  (match e
         [`(,xlet* () ,e0)
          (t-desugar e0 env)]
         [`(,_ ([,x ,e0] . ,rest) ,e1)
          (t-desugar
            `(%%let ([,x ,e0]) (%%let* ,rest ,e1))
            env)]))

(define (t-guard e env)
  (match e
         [`(,xguard (,x) ,e0)
          (t-desugar e0 env)]
         [`(,xguard (,x ,clauses ... [else ,eelse]) ,e0)
          (t-desugar
           `(%%let ([%old-handler %raise-handler])
                   (%%let ((%cc (%%call/cc (%%lambda (k) k))))
                          (%%cond [(%%prim procedure? %cc)
                                   (%%dynamic-wind
                                    (%%lambda () (%%set! %raise-handler (%%lambda (x) (%cc (%%prim cons x (%%quote ()))))))
                                    (%%lambda () ,e0)
                                    (%%lambda () (%%set! %raise-handler %old-handler)))]
                                  [(%%prim cons? %cc)
                                   (%%let ([,x (%%prim car %cc)])
                                          (%%cond ,@clauses [else ,eelse]))])))
           env)]
         [`(,xguard (,x ,clauses ...) ,e0)
          (t-desugar
            `(%%guard (,x ,@clauses [else (%%raise ,x)]) ,e0)
            env)]))

(define (t-raise e env)
  (match e
         [`(,xraise ,e0)
          (t-desugar
           `(%raise-handler ,e0)
           env)]))

(define (t-call/cc e env)
  (match e
         [`(,xccc ,e0)
          `(call/cc
            ,(t-desugar
              `(%%lambda (%k)
                 (,e0 (%%let ([%saved-stack %wind-stack])
                             (%%lambda (%x)
                               (%%begin
                                 (%%if (%%prim eq? %saved-stack %wind-stack)
                                       (%%prim void)
                                       (%do-wind %saved-stack))
                                 (%k %x))))))
              env))]))


(define (t-let/cc e env)
  (match e
         [`(,xletcc ,x ,e0)
          (t-desugar `(%%call/cc (%%lambda (,x) ,e0))
                     env)]))


(define (t-dynamic-wind e env)
  (match e
         [`(,xwind ,e0 ,e1 ,e2)
          (t-desugar
           `(%%let ([%pre ,e0] [%body ,e1] [%post ,e2])
                   (%%begin (%pre)
                            (%%set! %wind-stack (%%prim cons (%%prim cons %pre %post) %wind-stack))
                            (%%let ([%v (%body)])
                                   (%%begin (%%set! %wind-stack (%%prim cdr %wind-stack))
                                            (%post)
                                            %v))))
           env)]))

(define (t-delay e env)
  (match e
         [`(,xdelay ,e0)
          (t-desugar
           `(%%prim list (%%quote %%promise) (%%lambda () ,e0) (%%prim make-vector (%%quote 2) (%%quote #f)))
           env)]))

(define (t-force e env)
  (match e
         [`(,xforce ,e0)
          (define t (gensym 'promise))
          (define tt (gensym 'promise))
          (t-desugar 
            `(%%let ([,t ,e0])
                    (%%if (promise? ,t)
                          (%%if (%%prim eq? (%%quote %%promise) (%%prim car ,t))
                                (%%if (%%prim vector-ref (%%prim third ,t) (%%quote 0))
                                      (%%prim vector-ref (%%prim third ,t) (%%quote 1))
                                      (%%let ([,tt ((%%prim second ,t))])
                                             (%%begin (%%prim vector-set! (%%prim third ,t) (%%quote 0) (%%quote #t))
                                                      (%%prim vector-set! (%%prim third ,t) (%%quote 1) ,tt)
                                                      ,tt)))
                                (%%raise (%%quote "value is not a promise")))
                          ,t))
            env)]))

(define (t-cond e env)
  (match e
         [`(,xcond) (t-desugar `(%%prim void) env)]
         [`(,xcond (else ,e0)  . ,rest) (t-desugar e0 env)]
         [`(,xcond (,e0) . ,rest)
          (define t0 (gensym 'cond))
          (t-desugar
            `(%%let ([,t0 ,e0])
               (%%if ,t0
                   ,t0
                   (%%cond . ,rest)))
            env)]
         [`(,xcond (,e0 => ,e1) . ,rest)
          (define t0 (gensym 'cond))
          (t-desugar
            `(%%let ([,t0 ,e0])
               (%%if ,t0
                   (,e1 ,t0)
                   (%%cond . ,rest)))
            env)]
         [`(,xcond (,e0 ,e1) . ,rest)
          (t-desugar
            `(%%if ,e0 ,e1 (%%cond . ,rest))
            env)]))

(define (t-case e env)
  (match e
         [`(,xcase ,keye ,clauses ...)
          #:when (not (symbol? keye))
          (define t (gensym 'case-key))
          (t-desugar `(%%let ([,t ,keye]) (%%case ,t . ,clauses))
                     env)]
         
         [`(,xcase ,key) (t-desugar `(%%prim void) env)]
         [`(,xcase ,key (else ,e0)  . ,rest) (t-desugar e0 env)]
         [`(,xcase ,key ((,(? datum? ds) ...) ,e0) . ,rest)
          (t-desugar
           `(%%if (%%prim memv ,key (%%quote ,ds))
                  ,e0
                  (%%case ,key . ,rest))
            env)]))

(define (t-or e env)
  (match e
         [`(,xor) (t-desugar `(%%quote #f) env)]
         [`(,xor ,e0) (t-desugar e0 env)]
         [`(,xor ,e0 . ,es)
          (define t (gensym 'or))
          (t-desugar `(%%let ([,t ,e0]) (%%if ,t ,t (%%or . ,es)))
                     env)]))

(define (t-and e env)
  (match e
         [`(,xand) (t-desugar `(%%quote #t) env)]
         [`(,xand ,e0) (t-desugar e0 env)]
         [`(,xand ,e0 . ,es)
          (t-desugar `(%%if ,e0 (%%and . ,es) (%%quote #f))
                     env)]))

(define (t-begin e env)
  (match e
         [`(,xbegin) (t-desugar `(%%prim void) env)]
         [`(,xbegin ,e0) (t-desugar e0 env)]
         [`(,xbegin ,e0 . ,es)
          (t-desugar
            `(%%let ([,(gensym '_) ,e0])
               (%%begin . ,es))
            env)]))

(define (t-let e env)
  (match e
         [`(,xlet ([,xs ,es] ...) ,e0)
          `(let ,(map (lambda (x e) `(,(prefix x) ,(t-desugar e env))) xs es)
             ,(t-desugar e0 (ext-env env xs (map prefix xs))))]
         
         [`(,xlet ,loop ([,xs ,es] ...) ,e0)
          (t-desugar
           `(%%letrec* ([,loop (%%lambda ,xs ,e0)])
                       (,loop . ,es))
           env)]))

(define (t-lambda e env)
  (match e
         [`(,xlambda (,(? symbol? xs) ...) ,e0)
          `(lambda ,(map prefix xs) ,(t-desugar e0 (ext-env env xs (map prefix xs))))]
         
         [`(,xlambda ,(? symbol? x) ,e0)
          `(lambda ,(prefix x) ,(t-desugar e0 (ext-env env (list x) (list (prefix x)))))]
         
         [`(,xlambda ,args ,e0)
          (define al (flatten args))
          (define xs (drop-right al 1))
          (define y (last al))
          (define tx (gensym 'vararg))
          `(lambda ,tx
             ,(t-desugar
               `(%%let* ,(foldr (lambda (x binds) `([,x (car ,tx)] [,tx (cdr ,tx)] . ,binds))
                              `([,y ,tx])
                              xs)
                  ,e0)
               env))]))

(define (t-apply e env)
  (match e
         [`(,xapply ,e0 ,e1)
          `(apply ,(t-desugar e0 env)
                  ,(t-desugar e1 env))]))

(define (t-when e env)
  (match e
         [`(,xwhen ,e0 ,e1)
          `(if ,(t-desugar e0 env)
               ,(t-desugar e1 env)
               (prim void))]))

(define (t-unless e env)
  (match e
         [`(,xunless ,e0 ,e1)
          `(if ,(t-desugar e0 env)
               (prim void)
               ,(t-desugar e1 env))]))

(define (t-if e env)
  (match e
         [`(,xif ,e0 ,e1)
          `(if ,(t-desugar e0 env)
               ,(t-desugar e1 env)
               (prim void))]
         [`(,xif ,e0 ,e1 ,e2)
          `(if ,(t-desugar e0 env)
               ,(t-desugar e1 env)
               ,(t-desugar e2 env))]))

(define (t-set! e env)
  (match e
         [`(,xset! ,x ,e0)
          `(set! ,(prefix x)
                 ,(t-desugar e0 env))]))

(define (t-quote e env)
  (match e
         [`(,xquote ,d)
          `(quote ,d)]))

(define (t-prim e env)
  (match e
         [`(,xprim ,op ,es ...)
          (define check (check-args op es))
          `(prim ,op . ,(map (lambda (e) (t-desugar e env)) check))]))

(define (t-apply-prim e env)
  (match e
         [`(,xapply-prim ,op ,es ...)
          (define check (check-args op es))
          `(apply-prim ,op . ,(map (lambda (e) (t-desugar e env)) check))]))

;***********************
;Check arguments function that makes sure each primitive recieves the right amount of arguments.
;If not then an error is thrown.
(define (check-args op args)
  (match op
    ['hash
     (if (= (length args) 2)
         (if (= (length (first args)) (length (second args)))
             args
             (error "The amount of keys and values provided for hash, must be equal"))
         (if (= (length args 0))
             args
             (if (> (length args) 2)
                 (error "run-time error: Too many arguments for hash")
                 (error "run-time error: Not enough arguments for hash"))))]
    ['hash-ref
     (if (= (length args) 2)
         args
         (if (> (length args) 2)
             (error "run-time error: Too many arguments for hash-ref")
             (error "run-time error: Not enough arguments for hash-ref")))]
    ['hash-set
     (if (= (length args) 3)
         args
         (if (> (length args) 3)
             (error "run-time error: Too many arguments for hash-set")
             (error "run-time error: Not enough arguments for hash-set")))]
    
    ['hash-remove
     (if (= (length args) 2)
         args
         (if (> (length args) 2)
             (error "run-time error: Too many arguments for hash-remove")
             (error "run-time error: Not enough arguments for hash-remove")))]
    ['eq?
     (if (= (length args) 2)
         args
         (if (> (length args) 2)
             (error "run-time error: Too many arguments for eq?")
             (error "run-time error: Not enough arguments for eq?")))]
    ['eqv?
     (if (= (length args) 2)
         args
         (if (> (length args) 2)
             (error "run-time error: Too many arguments for eqv?")
             (error "run-time error: Not enough arguments for eqv?")))]
    ['number?
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for number?")
             (error "run-time error: Not enough arguments for number?")))]
    ['integer?
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for integer?")
             (error "run-time error: Not enough arguments for integer?")))]
    ['void?
     (if (= (length args) 2)
         args
         (if (> (length args) 2)
             (error "run-time error: Too many arguments for void?")
             (error "run-time error: Not enough arguments for void?")))]
    ['procedure?
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for procedure?")
             (error "run-time error: Not enough arguments for procedure?")))]
    ['null?
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for null?")
             (error "run-time error: Not enough arguments for null?")))]
    ['cons?
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for cons?")
             (error "run-time error: Not enough arguments for cons?")))]
    ['cons
     (if (= (length args) 2)
         args
         (if (> (length args) 2)
             (error "run-time error: Too many arguments for cons")
             (error "run-time error: Not enough arguments for cons")))]
    ['car
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for car")
             (error "run-time error: Not enough arguments for car")))]
    ['cdr
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for cdr")
             (error "run-time error: Not enough arguments for cdr")))]
    ['not
     (if (= (length args) 1)
         args
         (if (> (length args) 1)
             (error "run-time error: Too many arguments for not")
             (error "run-time error: Not enough arguments for not")))]
    ['=
     (if (> (length args) 0)
         args
         (error "run-time error: Not enough arguments for ="))]
    ['-
     (if (> (length args) 1)
         args
         (if (> (length args) 0)
             (cons 0 args)
             (error "run-time error: Not enough arguments for -")))]
    ['+
     (if (= (length args) 0)
         (list 0 0)
         (if (= (length args) 1)
             (cons 0 args)
             args))]
    ['*
     (if (= (length args) 0)
         (list 1 1)
         (if (= (length args) 1)
             (cons 1 args)
             args))]
    ['/
     (if (> (length args) 0)
         (if (member '0 args)
             (error "Division by zero")
             (if (= (length args) 1)
                 (cons 1 args)
                 args))
         (error "run-time error: Not enough arguments for /"))]
    ['>
     (if (> (length args) 1)
         args
         (error "run-time error: Not enough arguments for >"))]
    ['<
     (if (> (length args) 1)
         args
         (error "run-time error: Not enough arguments for <"))]
    ['>=
     (if (> (length args) 1)
         args
         (error "run-time error: Not enough arguments for >="))]
    ['<=
     (if (> (length args) 1)
         args
         (error "run-time error: Not enough arguments for <="))]
    [else args]))

; The top level desugaring function
; takes an e, env and either dispatches on the function in the env
; or matches a symbol?, prim-op, or normal application
(define (t-desugar e env)  ; (pretty-print `(t-desugar ,e ,env))
  (match e
         ; match a transformer in env
         [`(,(? (lambda (x) (procedure? (hash-ref env x #f))) x) . ,rest)
          ((hash-ref env x) e env)]

         ; primitive (optimized case)
         [`(,(? prim? op) ,es ...)
          #:when (eq? #t (hash-ref env op #f))
          (t-desugar `(%%prim ,op . ,es)
                     env)]

         ; optional renaming by the env
         [(? symbol? x)
          (define xx (hash-ref env x #f))
          (match xx
                 [#f (if (hash-ref env (prefix x) #f)
                         x
                         (error (string-append "Use of undefined variable: " (symbol->string x))))]
                 [#t (t-desugar `(%%lambda args (%%apply-prim ,x args)) env)]
                 [(? symbol?) xx]
                 [(? procedure?)
                  (error (format "variable ~a bound to transformer in environment" x))])]         

         ; untagged application
         [`(,es ...)
          (if (hash-has-key? env (car es))
              (map (lambda (e) (t-desugar e env)) es)
              (if (pair? (car es))
                  (match (car es)
                    [`(lambda ,args ... ,body)
                     (if (= (length args) (length (cdr es)))
                         (map (lambda (e) (t-desugar e env)) es)
                         (if (< (length (cdr es)) (length args))
                             (raise "run-time error: Function is provided too many arguments")
                             (raise "run-time error: Function is provided too few arguments")))]
                    [else (map (lambda (e) (t-desugar e env)) es)])
                  (error (string-append "run-time error: Non function value is applied: " (symbol->string (car es))))))]
          ;(map (lambda (e) (t-desugar e env)) es)]

         [else (pretty-print `(bad-term ,e ,env))
               (error 'bad-term)]))



(define (desugar e)
  (define (wrap e)
    `(let* ([promise? (lambda (p) (and (cons? p) (eq? (car p) (quote %%promise))))]
            [%raise-handler '()]
            [%wind-stack '()]
            [%common-tail (lambda (x y)
                            (let ((lx (length x))
                                  (ly (length y)))
                              (let loop ([x (if (> lx ly) (drop x (- lx ly)) x)]
                                         [y (if (> ly lx) (drop y (- ly lx)) y)])
                                (if (eq? x y)
                                    x
                                    (loop (cdr x) (cdr y))))))]
            [%do-wind (lambda (new)
                        (let ([tail (%common-tail new %wind-stack)])
                          (begin
                            (let f ((l %wind-stack))
                              (if (not (eq? l tail))
                                  (begin
                                    (set! %wind-stack (cdr l))
                                    ((cdr (car l)))
                                    (f (cdr l)))))
                            (let f ([l new])
                              (if (not (eq? l tail))
                                  (begin
                                    (f (cdr l))
                                    ((car (car l)))
                                    (set! %wind-stack l)))))))])
           ,e))
  
  ; The environment starts with procedures for forms we desugar
  (define reserved-env (hash
                    'letrec* t-letrec* 
                     'letrec t-letrec 
                     'let* t-let* 
                     'guard t-guard 
                     'raise t-raise
                     'dynamic-wind t-dynamic-wind
                     'delay t-delay 
                     'force t-force 
                     'cond t-cond
                     'case t-case
                     'and t-and
                     'or t-or
                     'begin t-begin
                     'let t-let
                     'if t-if
                     'when t-when
                     'unless t-unless
                     'lambda t-lambda
                     'quote t-quote
                     'apply t-apply
                     'prim t-prim
                     'call/cc t-call/cc
                     'let/cc t-let/cc
                     'apply-prim t-apply-prim
                     'set! t-set!))
  ; add a set of special aliases for the core forms we can use safely in desugaring
  ; (a form like lambda or letrec can be shadowed)
  (define aliases-env (foldl (lambda (k env) (hash-set env
                                                       (string->symbol (string-append "%%" (symbol->string k)))
                                                       (hash-ref env k)))
                             reserved-env
                             (hash-keys reserved-env)))
  ; primitive operations start as #t and new (user) bindings are #f 
  (define prims-env (foldl (lambda (p env) (hash-set env p #t))
                           aliases-env
                           (prims->list)))
  (t-desugar (wrap e) prims-env))




