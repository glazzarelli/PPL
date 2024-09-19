```scheme
(define (example)
  (call/cc (lambda (outer-cont)
             (display "Before inner call/cc\n")
             (call/cc (lambda (inner-cont)
                        (display "Inside inner call/cc\n")
                        (outer-cont "Returned from inner to outer directly"))))
             (display "After inner call/cc\n"))
  (display "After outer call/cc\n"))

(example)
```

