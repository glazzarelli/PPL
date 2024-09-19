## Variable length arguments

What's the output of this snippet?

```scheme
(define (mix f . args)
  (if (null? args)
      '(f)
      (list (car args) (mix f (cdr args)) (car args))))


(mix 'f 1 2 3)
```

--- 

If you though it'd be `'(1 (2 (3 (f) 3) 2) 1)` you are not correct, instead this computation does not end, why? 
In this case you can think the definition of `mix f . args` as equivalent to the call `mix f (list args)`.
That means that when we call ```(mix f (cdr `(1 2 3)))```  we end with `mix f (list (cdr args))` that is:
```scheme
(mix f . (cdr `(1 2 3)))      ; which is = to
(mix f (list (cdr `(1 2 3)))) ; which is = to
(mix f (list `(2 3)))         ; which is = to
(mix f ((2 3)))              
```
At the next iteration we have that ```cdr ((2 3))``` which is the empty list and from that point on the it will call it self an infinite number of times since in `mix f (())` args are not null, but a list contain the null list.

To sum it up, the dot `.` notation is useful when we want to pass a variable number of arguments to a function without using a list, but inside the body of the function we can treat `args` as a list. 

The only thing that changes between `(mix f alist)` and `(mix f . args)` in how we call (or *recursive* call) the function, inside the body `alist` and `args` will be the same:
```scheme
(define (mix f . args)
  (cons f args))

(define (mix2 f alist)
  (cons f alist))

(mix 'f 1 2 3)         ; '(f 1 2 3)
(mix2 'f (list 1 2 3)) ; '(f 1 2 3)
```

The last question now is, how do we can recursively call the first `mix` function without having the nested list problem? 
The easiest way is to treat `args` as list and in order to do that we can create an auxiliary function:

```scheme
(define (mix f . args)
  (define (mix-aux f args)
    (if (null? args)
        (list f)
        (list (car args) (mix-aux f (cdr args)) (car args))))
  (mix-aux f args))

(mix 'f 1 2 3) ;; => '(1 (2 (3 (f) 3) 2) 1)
```

A shorter approach with `apply`:
```scheme
(define (mix f . args)
  (if (null? args)
      '(f)
      (list (car args) (apply mix (cons f (cdr args))) (car args))))

(mix 'f 1 2 3)
```
Here we can we how each element, after the first (element) `mix`, can be considered the list of arguments, where `f . args` is a way to pattern match the head `f` from the rest of list `args`.

