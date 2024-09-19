
Define a `let**` construct that behaves like the standard `let*`, but gives to variables provided without a binding the value of the last defined variable. It also contains a default value, stated by a special keyword `def:`, to be used if the first variable is given without binding.

Why bad syntax?
```scheme
(define-syntax let**
  (syntax-rules (def:)
    ((_ def: dfvalue () body ...)
     body ...)
    
    ((_ def: dfvalue ((var value) . rest) body ...)
     (let ((var value))
       (let** def: value rest body ...)))
    
    ((_ def: dfvalue (var . rest) body ...)
     (let ((var dfvalue))
       (let** def: dfvalue rest body ...)))))

     



(let** def: #f
  (a (b 1) (c (+ b 1)) d (e (+ d 1)) f)
  (list a b c d e f))
  
```


The problem is the empty parenthesis `()` in the first rule, correct one:

```scheme
(define-syntax let**
  (syntax-rules (def:)
    ((_ def: dfvalue () body ...)
     body ...)
    
    ((_ def: dfvalue ((var value) . rest) body ...)
     (let ((var value))
       (let** def: value rest body ...)))
    
    ((_ def: dfvalue (var . rest) body ...)
     (let ((var dfvalue))
       (let** def: dfvalue rest body ...)))))
```