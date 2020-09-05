(define sum?
  (lambda(x)
    (and (pair? x) (equal? (car x) '+))
  )
)

(define sub?
  (lambda(x)
    (and (pair? x) (equal? (car x) '-))
  )
)

(define pro?
  (lambda(x)
    (and (pair? x) (equal? (car x) '*))
  )
)

(define exp?
  (lambda(x)
    (and (pair? x) (equal? (car x) '**))
  )
)

(define var?
  (lambda(x)
    (symbol? x)
  )
)

(define samevar?
  (lambda(a b)
    (and (var? a) (var? b) (equal? a b))
  )
)

(define make_sum
  (lambda(a b)
    (list '+ a b)
  )
)

(define make_sub
  (lambda(a b)
    (list '- a b)
  )
)

(define make_pro
  (lambda(a b)
    (list '* a b)
  )
)

(define make_exp
  (lambda(a b)
    (list '** a b)
  )
)

(define ** expt)

(define myappend
  (lambda(ls)
    (apply append ls)
 )
)

(define diff
  (lambda(fx)
    (cond ((number? fx) 0)
          ((var? fx) 1)
          ((sum? fx) (myappend(list (list'+) (map(lambda(f) (diff f)) (cdr fx)))))
          ((sub? fx) (myappend(list (list'-) (map(lambda(f) (diff f)) (cdr fx)))))
          ((pro? fx) (list '+ (list '* (cadr fx) (diff  (caddr fx))) (list '*  (diff  (cadr fx)) (caddr fx))))
          ((exp? fx) (list '* (caddr fx) (list '*  (diff (cadr fx)) (list '** (cadr fx) (- (caddr fx) 1)))))
    )
  )
)

(define tangent
  (lambda(fx num)
    (let ((fxnum ((eval `(lambda (x) ,(diff fx)) (interaction-environment)) num) )
          (Y ((eval `(lambda (x) ,fx) (interaction-environment)) num)))
    (list '+ (list '* fxnum 'x) (- Y (* fxnum num)))
    )
  )
)

(define diff2
  (lambda(fx d)
    (cond ((number? fx) 0)
          ((var? fx)
           (if (samevar? fx d) 1 0))
          ((sum? fx)
           (make_sum (diff2 (cadr fx) d) (diff2 (caddr fx) d))
          )
          ((sub? fx)
           (make_sub (diff2 (cadr fx) d) (diff2 (caddr fx) d))
          )
          ((pro? fx)
           (make_sum
            (make_pro (cadr fx) (diff2 (caddr fx) d))
            (make_pro (diff2 (cadr fx) d) (caddr fx))
           )
          )
          ((exp? fx)
           (make_pro
            (make_pro (caddr fx)
                      (make_exp (cadr fx)
                                (- (caddr fx) 1)))
            (diff2 (cadr fx) d)))
     ))
)

(define make_nonzero
  (lambda(lst)
    (if (equal? lst 0)
        '()
        (list lst)
    )
  )
)

(define simple+
  (lambda(lst)
    (let ((non-zero-list (myappend (map (lambda(t) (make_nonzero t)) lst) )))
        (cond ((null? non-zero-list) 0)
              ((null? (cdr non-zero-list)) (car non-zero-list))
              (else (myappend (list (list '+) non-zero-list)))
        )
    )
  )
)

(define simple-
  (lambda(lst)
    (let ((non-zero-list (myappend (map (lambda(t) (make_nonzero t)) lst) )))
        (if (null? non-zero-list)
            (car non-zero-list)
            (myappend (list (list '-) non-zero-list))
         )
     )
  )
)


(define simple*
  (lambda(lst)
    (let ((p (car lst)) (q (cadr lst)))
      (cond ((or (equal? p 0) (equal? q 0)) 0)
            ((equal? p 1) q)
            ((equal? q 1) p)
            (else (myappend (list (list '*) lst)))
      )
    )
  )
)

(define simple**
  (lambda(lst)
    (let ((p (car lst)) (q (cadr lst)))
      (cond ((equal? q 0) 1)
            ((equal? q 1) p)
            (else (myappend (list (list '**) lst)))
      )
    )
  )
)

(define simple
  (lambda(fdx)
    (cond ((number? fdx) fdx)
          ((var? fdx) fdx)
          ((sum? fdx)
           (simple+ (cdr (make_sum (simple (cadr fdx)) (simple (caddr fdx)))))
          )
          ((sub? fdx)
           (simple- (cdr (make_sub (simple (cadr fdx)) (simple (caddr fdx)))))
          )
          ((pro? fdx)
           (simple* (cdr (make_pro (simple (cadr fdx)) (simple (caddr fdx)))))
          )
          ((exp? fdx)
           (simple** (cdr (make_exp (simple (cadr fdx)) (simple (caddr fdx)))))
          )
     )
   )
)

