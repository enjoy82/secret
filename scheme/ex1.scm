(define TREE '(1 (2 (3 4)) 6 (7 8 9)))

(define mymap
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l))
              (mymap f (cdr l))))
    ))

(define map-tree
  (lambda (fn tree)
    (cond((null? tree) '())
        ((pair? tree) (cons(map-tree fn (car tree))
                      (map-tree fn (cdr tree))))
        (else  (fn tree))
    )
  )
)

(define map-tree2
  (lambda (fn tree)
    (cond((null? tree) '())
        ((pair? tree) (map (lambda (t) (map-tree2 fn t)) tree ))
        (else  (fn tree))
    )
  )
)