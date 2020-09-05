(define alphabet
  (read (open-input-file "/class/scheme/alphabet"))
)

(define tokugawa
  (read (open-input-file "/class/scheme/tokugawa"))
)

(define myappend
  (lambda(ls)
    (apply append ls)
 )
)

(define get-depth
  (lambda(tree depth)
    (cond((null? tree) '())
        ((= depth 1)
         (map car (cdr tree))
        )
        (else (myappend(map (lambda (t) (get-depth t (- depth 1))) (cdr tree)))
        ) 
    )
  )
)

(define search
  (lambda(tree atom depth)
    (if (member atom (get-depth tree depth))
        depth
        (search tree atom (+ depth 1))
    )
  )
)

(define get-cousin
  (lambda(tree atom)
    (get-depth tree (search tree atom 1))
  )
)

(define get-ans
  (lambda(tree atom ans)
    (cond ((equal? atom (car tree))  (append ans (list (car tree))))
          ((null? tree) '())
          (else
           (if (list? ans)
               (myappend (map (lambda(t) (get-ans t atom (append  ans (list (car tree))) )) (cdr tree)))
               (myappend(map (lambda(t) (get-ans t atom (append (list ans) (list (car tree))) )) (cdr tree))))
           )
    )
  )
)

(define get-path
  (lambda(tree atom)
    (myappend (map (lambda(t) (get-ans t atom (car tree))) (cdr tree)))
  )
)