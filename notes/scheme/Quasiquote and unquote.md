A function that use `quasiquote` and `unquote` to get the prefix of a list.
```scheme
(define (l-pre n l)
  (define (l-pre-aux acc k list)
    (if (eqv? 0 k)
        acc
        (if (null? list)
            acc
            (l-pre-aux (append acc `(,(car list))) (- k 1) (cdr list)))))
  (l-pre-aux '() n l))
```