(define-syntax stream-cons
    (syntax-rules ()
        ((_ x y) (cons x (delay y)))
    ))

(define-syntax stream-car
    (syntax-rules ()
        ((_ x) (car x ))
    ))

(define-syntax stream-cdr
    (syntax-rules ()
        ((_ x) (force (cdr x)))
    ))

(define head (lambda (n L)
  (if (<= n 0) '()
      (cons (stream-car L) (head (- n 1) (stream-cdr L)))
      )))

(define numbers (lambda ()
  (letrec ((stream
              (lambda (n) (stream-cons n (stream (+ n 1))))
          ))
          (stream 2))))

(define (stream-filter pred s)
  (cond ((null? s) nil)
        ((pred (stream-car s))
         (stream-cons (stream-car s)
                      (stream-filter pred (stream-cdr s))
        ))
        (else
         (stream-filter pred (stream-cdr s))
        )
 )
)



(define (sieve stream)
  (stream-cons (stream-car stream)
        (sieve (stream-filter
                       (lambda(x) (not (zero? (modulo x (stream-car stream)))))
                       (stream-cdr stream)
                      )
        )
  )
)

(define primes
  (lambda(x)
    (head x (sieve (numbers)))
  )
)