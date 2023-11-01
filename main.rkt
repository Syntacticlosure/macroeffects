#lang racket

(require (for-syntax syntax/parse)) 
(provide transformer-ref
         space-introducer
         with-implicit-macro-args
         call-macro)

(define transformer-ref (make-parameter identity))
(define space-introducer (make-parameter identity))
(define current-introducer (make-parameter #f))

(define implicit-macro-args (make-parameter (seteq)))

(define-syntax (with-implicit-macro-args stx)
  (syntax-parse stx
    [(_ ([p stx] ...) bodies ...)
     #:with (p-id ...) (generate-temporaries #'(p ...))
     #'(let ([p-id p] ...)
         (parameterize ([p-id stx] ...
                        [implicit-macro-args (set-union (implicit-macro-args) (seteq p-id ...))])
           bodies ...))]))

(define (introduce-implicit-macro-args introducer body-thunk)
  (define (apply-introducer introducer stx)
    (if (syntax? stx)
        (introducer stx)
        (send stx introduce introducer)))
  
  (define macro-args (set->list (implicit-macro-args)))

  (define (loop-exit)
    (let loop-exit ([macro-args macro-args])
      (if (null? macro-args)
          (void)
          (begin (apply-introducer introducer ((car macro-args)))
                 (loop-exit (cdr macro-args))))))
  (let loop ([macro-args macro-args])
    (if (null? macro-args)
        (begin0 (body-thunk)
                (loop-exit))
        (parameterize ([(car macro-args) (apply-introducer introducer ((car macro-args)))])
          (loop (cdr macro-args))))))
         
(define (syntax-local-apply-transformer transformer stx)
  (define macro-introducer (make-syntax-introducer))
  (define local-introducer (or (current-introducer) syntax-local-introduce))
  (define introducer (λ (stx) (local-introducer (macro-introducer stx))))
  (introduce-implicit-macro-args introducer
                                 (λ () (parameterize ([current-introducer macro-introducer])
                                         (introducer (transformer (introducer stx)))))))
                                      

(define (syntax-local-transformer id)
  (define in-space-id ((space-introducer) id))
  (define id-value (syntax-local-value in-space-id))
  (and id-value ((transformer-ref) id-value)))

(define (call-macro macro-id arg-stx)
  (define transformer (if (procedure? macro-id) 
                          macro-id
                          (syntax-local-transformer macro-id)))
  (unless transformer
    (raise-argument-error 'macro-id "an identifier which bound to a transformer"))
  (syntax-local-apply-transformer transformer
                                  arg-stx))


