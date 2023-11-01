#lang racket
(require (for-syntax "main.rkt" syntax/parse racket racket/syntax))

(begin-for-syntax
  (define myclass-meta%
    (class object%
      (super-new)
      (define properties '())
      (define methods '())
      (define constructor '())

      (define/public (introduce introducer)
        (set! properties (map introducer properties))
        (set! methods (map introducer methods))
        this)
      (define/public (add-property name val)
        (set! properties (cons #`(#,name #,val) properties)))
      (define/public (add-method name val)
        (set! methods (cons #`(#,name #,val) methods)))
      (define/public (add-constructor clause)
        (set! constructor (cons clause constructor)))
      (define/public (gen)
        (syntax-parse methods
          [((method _) ...) 
        #`(let #,properties
            (letrec #,methods
              #,@(reverse constructor)
              (λ (method-name . args)
                (match method-name
                  ['method (apply method args)] ...))))]))))
  (define myclass-meta (make-parameter #f)))

(define-syntax (property stx)
  (syntax-parse stx
    [(_ name:id val:expr)
     (send (myclass-meta) add-property #'name #'val)
     (define getter (format-id #'ctx "get-~a" #'name)) 
     (send (myclass-meta) add-method getter #'(λ () name))
    #'#f]))

(define-syntax (method stx)
  (syntax-parse stx
    [(_ name:id val:expr)
     (send (myclass-meta) add-method #'name #'val)
     #'#f]))


(define-syntax (myclass stx)
  (syntax-parse stx
    [(_ clauses ...)
     (with-implicit-macro-args
         ([myclass-meta (new myclass-meta%)])
       (for ([c (syntax-e #'(clauses ...))])
         (syntax-parse c
           [((~and op (~or (~literal property) (~literal method))) . args)
            (call-macro #'op
                        c)]
           [_ (send (myclass-meta) add-constructor c)]))
       (send (myclass-meta) gen))]))
     
(define c (myclass
           (property a 1)
           (property b 2)

           (define a 3)
           (method p (λ () (+ a b)))
           (p)
           (display a)
           ))
(c 'get-a)

