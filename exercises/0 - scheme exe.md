## Recursion and tail-recursion
### List length

Define a Scheme function `my-len` that calculates the length of a given list. You should implement this function using a tail-recursive helper function. Your function should take one argument, which is the list for which you want to find the length.

```scheme
(define (len L)
  (define (tail-len L k)
    (if (null? L) k
        (tail-len (cdr L) (+ 1 k))))
  (tail-len L 0))
```

Non tail-recursive version:

```scheme
(define (my-len L)
  (if (null? L)
      0
      (+ 1 (my-len (cdr L))))
)
```

>[!note] tail-recursion
>The last operation in the tail-recursive `tail-len` is calling `tail-len` itself while in the not tail-recursive version the function `my-len` does and additional operation `(+ 1 ...).`

Non recursive version:

```scheme
(define (my-len L)
  (let loop ((l L) (k 0))
    (if (pair? l)
      (loop (cdr l) (+ k 1))
      k)
    ))
```

To check whether a list is empty:

```scheme
(null? '()) ; => #t
(pair? '()) ; => #f
```


### List prefix

Define a procedure `prefix` that generates a new list containing the first 'n' elements of a given list.

```scheme
(define (prefix n L)
  (define (prefix-tail pre n L)
    (if (= n 0)
        pre
        (if (pair? L)
            (prefix-tail (append pre (list (car L))) (- n 1) (cdr L))
            (error "ERROR: prefix length is bigger than list lenght"))))
    (prefix-tail '() n L))
```

>[!note] `cons` vs `append`
>The `append` procedure takes two lists as arguments and append the second list to the first: it does not accept other types of arguments.
>On the other hand `cons` handles more types, and can be used create pairs of heterogeneous types. Since list in Scheme are save as nested pairs, `cons` can create a list from an element and a list:
>```scheme
>(define l '(1 2 3 4)); is saved as (1.(2.(3.(4.'()))))
>(cons 0 l)           ; => '(0 1 2 3 4)
> ```
> Using `cons` we can *prepend* a value to a list.

Here we use `cons` instead of `append` to define the `prefix` procedure:
```scheme
(define (prefix* n L)
  (if (= n 0)
      (list (car L))
      (cons (car L) (prefix* (- n 1) (cdr L)))))
```
This function takes the prefix of length $n+1$ of a list L, in this case we don't consider the case where $n$ is less than the list length.


### List reference

Define a procedure `ref` that takes a number and list and the return the item whose index is equal to the number.

```scheme
(define (ref k L)
  (if (= k 0)
      (car L)
      (ref (- k 1) (cdr L))))
```
The function is tail-recursive.


### Range

Define a `range` procedure that returns a list of numbers:
- If given one argument `s`, it generates a list of integers from 0 to `s - 1`.
```scheme
(range 3)  (0 1 2 3)
```
- If given two arguments `s` and `e`, it generates a list of integers from `s` to `e - 1`.
  ```scheme
(range 2 3)   (2 3)
```

A possible tail-recursive implementation:
```scheme
(define (range s . e)
  (define (tail-range-one s acc) ; one param provided: end = s
        (if (= s -1)
            acc
            (tail-range-one (- s 1) (cons s acc))))
  (define (tail-range-two s e acc) ; two parameters provided
    (if (= e  (- s 1))
        acc
        (tail-range-two s (- e 1) (cons e acc))))
  (if (null? e)
      (tail-range-one s '())
      (tail-range-two s (car e) '())))
```

Here I defined two different implementations of the same recursive procedure to differentiate between the two case, but a shorter implementation is possible after noting that `tail-range-one s '()'` is equal to `tail-range-two 0 s '()`:

```scheme
(define (range s . e)
  (define (tail-range s e acc)
    (if (= e  (- s 1))
        acc
        (tail-range s (- e 1) (cons e acc))))
  (if (null? e)
      (tail-range 0 s '())
      (tail-range s (car e) '())))
```


### While

Implement a recursive function in Scheme that simulates the behavior of a while loop. This function, named `while`, will take two arguments: a condition function `c` and a body function `b`. The `while` function should execute the body function `b` as long as the condition function `c` returns true.

```scheme
(define (while condition body)
  (let loop ()
    (when (condition)
      ((lambda ()
         (body)
         (loop)))
      )))

;; helper to test the procedure
(define (test-while)
  (let ((x 0))
    (while (lambda () (< x 10))
         (lambda ()
           (displayln x)
           (set! x (+ 1 x))))))
```

This implementation works but there are a few issues:
1. The use of `let` to define a named loop is not necessary in this context. The `let` keyword is used to bind variables to values in a local scope, but here it's used to create a recursive function, which is better accomplished just with `define`.
2. Using a `lambda` to create an anonymous function that wraps the `body` and recursive call to `loop`is unnecessary.
   
A better recursive alternative:
```scheme
(define (while c b)
  (when (c)
    (b)
    (while c b)))
```

>[!note]- `lambda` vs `begin` for sequential evaluation
>We can use instead `lambda` instead of `begin` to evaluate expressions sequentially. 
>For example:
> ```scheme
> ((lambda ()
>    (display "This is ")
>    (display "sequential")))
> ;; instead of
> (begin
>   (display "This is ")
>   (display "sequential"))
> ```
> The first pair surrounding parentheses around `lambda` are the definition of the anonymous function, while the second set of surrounding parentheses immediately call this anonymous function.
> 
> In this case, the lambda functions and `begin` construct serve similar purposes, which is to allow multiple expressions to be evaluated sequentially. However, there are some differences between them:
> - **Flexibility**: Lambda functions can be passed around as arguments to other functions or returned as results, making them suitable for functional programming paradigms.
> - **Scope**: Lambda functions create their own scope, which can be beneficial when you want to encapsulate logic and avoid variable collisions.
> - **Readability**: Using `begin` can make the code more readable, as it explicitly shows the sequence of operations. 


### Reverse a List

Implement a procedure that takes a list `L` as argument and returns the list in reverse order.

A tail-recursive implementation:
```scheme
(define (reverse-l L)
  (define (reverse-l-tail L acc)
    (if (null? L)
        acc
        (reverse-l-tail (cdr L) (cons (car L) acc))))
  (reverse-l-tail L '()))
```

A non-tail recursive implementation:
```scheme
(define (tsil L)
  (if (null? L)
      '()
       (append (tsil (cdr L)) (list (car L)))))
```

Using `foldl`:
```scheme
(define (reverse-list lst)
 (foldl cons '() lst))
```

### Flatten a Nested List

Implement a procedure that given any list `L` as argument return the flatten version of `L`.

```scheme
(flat-l '(1 2 (3 4) 5 (6 7 (8 9))))) ;=> '(1 2 3 4 5 6 7 8 9) 
(flat-l '((1 2)(3 (4) (5)) ((6) (7 8)) (((9) 10))))
; => '(1 2 3 4 5 6 7 8 9 10)
```

>[!note] Combining `car` and `cdr`
>How to combine `car` and `cdr`:
> ```scheme
> (define nested-list
>   '(1 2 (3 4) 5 (6 7 (8 9))))
> 
> (car nested-list) ; returns 1
> (cadr nested-list) ; returns 2
> (caddr nested-list) ; returns (3 4)
> (cadddr nested-list) ; returns 5
> 
> ```
> Note that `(caddr x)` is equivalent to:
> ```scheme
> (car (cdr (cdr x)))
> ```

A solution could be:
```scheme
(define (flat-l L)
  (define (flat-l* L acc)
    (if (null? L)
        acc
        (if (pair? (car L))
            (append (flat-l* (car L) acc) (flat-l* (cdr L) '()))
            (flat-l* (cdr L) (append acc (list (car L)))))))
  (flat-l* L '()))
```

Tail-recursive(?) solution using `reverse` instead of `append`:
```scheme
(define (flat-l-v2 l)
  (define (flat-l* l acc)
    (cond ((null? l) acc)
          ((pair? (car l)) (flat-l* (cdr l) (flat-l* (car l) acc)))
          (else (flat-l* (cdr l) (cons (car l) acc)))))
  (reverse (flat-l* l '())))
```
`reverse` returns a list in reverse order.

>[!note] `cond` evaluation
>The `cond` form in scheme have short-cutting behavior:
> 
> ```scheme
> (define (test x)
>   (cond ((= x 1) 'one)
>         ((< x 2) (begin (display "Two") 'two))
>         ((< x 3) 'three)
>         (else 'other)))
> (test 1) ; => 'one
> ```

Another solution:
```scheme
(define (flat L)
  (if (null? L)
      '()
      (append (if (list? (car L))
                  (flat (car L))
                  (list (car L)))
              (flat (cdr L)))))
```



## Structure types

### Binary Tree

In this exercise we create a new a ***binary*** tree type in Scheme, display it, and apply functions to it, both in a non-destructive and a destructive manner.

1. Define the basic structures
Define a `node-base` structure and a `node` structure, the `node-base` doesn't have any children: is a leaf node that contains a mutable value. 

```scheme
(struct node-base
  ((value #:mutable)))

(struct node node-base
  (left
   right))
```

>[!note]- `struct` usage example
>
> ```scheme
> (struct being (
>   name            ; name is immutable
>   (age #:mutable) ; flag for mutability
> ))
> 
> (struct may-being
>     being ; being is the father
>     ((alive? #:mutable)) ; to be or not to be
> )
> ```
> A structure type `being` is defined with two fields: `name` and `age`. The `#:mutable` keyword indicates that the `age` field can be modified after an instance of the structure is created.
> This definition automatically creates several procedures related to the `being` structure:
> 1. `being`: A constructor function that takes as many arguments as the number of fields, and returns an instance of the structure type.
> 2. `being?`: A predicate function that takes a single argument and returns `#t` if it is an instance of the structure type, `#f` otherwise.
> 3. `being-name` and `being-age`: Accessor functions that extract the value of the corresponding field from an instance of the structure type.
> 4. `set-being-age!`: A mutator function that modifies the `age` field of an instance of the structure type. This function is only created because `age` was defined with the `#:mutable` keyword.
>    
> A structure subtype `may-being` is also defined, which extends the `being` type with an additional field `alive?`. In addition to the procedures created for the `being` structure, this definition creates similar procedures for the `may-being` structure, including a constructor, predicate, accessors, and a mutator for the `alive?` field. An instance of the `may-being` structure can be used with the predicate and accessors of the `being` structure.
> 
> ```scheme
> (define john (being "John" 25))
> (define may-john (may-being john #t))
> 
> (being-name john) ; returns "John"
> (being-age john) ; returns 25
> (being-alive? may-john) ; returns #t
> 
> (set-being-age! john 26)
> (set-may-being-alive?! may-john #f)
> 
> (being? john) ; returns #t
> (may-being? john) ; returns #f
> (being? may-john) ; returns #t because may-being is a subtype of being
> (may-being? may-john) ; returns #t
> ```
> 

2. Define the `node-nil`
In many tree algorithms, to allow *incomplete* trees we need a way to indicate that a node does not have a child. For this purpose, we define a special `node-nil` object and a `node-nil?` function that checks if a given object is a `node-nil` object.

```scheme
(define node-nil (node-base #f))
(define (node-nil? n)
  (eq? node-nil n))
```

3. Define some sample trees
Define some sample trees for testing purposes.

```scheme
(define t1
  (node 1
        (node 2
              (node-base 4) (node 5
                                  (node-base 6) node-nil))
        (node-base 3)))

;        1
;       / \
;      2   3
;     / \
;    4   5
;       /
;      6
;
;[1 [2 [4] [5 [6] []]] [3]]
```

```scheme
(define t2
  (node 1
        (node 2
              (node 4
                    (node-base 7) 
                    (node 8
                          (node-base 9) 
                          node-nil))
              (node 5
                    (node-base 6) 
                    node-nil))
        (node 3
              (node-base 10)
              (node 11
                    (node-base 12)
                    (node-base 13)))))
```

4. Define the `leaf?` function
Define a `leaf?` function that checks if a given node is a leaf node. A leaf node is a `node-base` that is not a `node`.

```scheme
(define (leaf? n)
  (and (node-base? n)
       (not (node? n))))
```

5. Define the `tree-display` function
Define a `tree-display` function that displays a tree in a readable format.

```scheme
(define (tree-display tree)
  (cond
    ((node-nil? tree) (display "[]"))
    ((leaf? tree) (begin
                    (display "[")
                    (display (node-base-value tree))
                    (display "]")))
    (else
     (begin
       (display "[")
       (display (node-base-value tree))
       (display " ")
       (tree-display (node-left tree))
       (display " ")
       (tree-display (node-right tree))
       (display "]")))))

(tree-display t1)
; => [1 [2 [4] [5 [6] []]] [3]]


(tree-display t2)
; [1 [2 [4 [7] [8 [9] []]] [5 [6] []]] [3 [10] [11 [12] [13]]]]
```

6. Define the `tree-map` function
Define a `tree-map` function that applies a given function to each value in the tree, producing a new tree.

```scheme
(define (tree-map f tree)
  (cond
    ((node-nil? tree) node-nil)
    ((leaf? tree) (node-base (f (node-base-value tree))))
    ((node? tree) (node
                   (f (node-base-value tree))
                   (tree-map f (node-left tree))
                   (tree-map f (node-right tree))))
    (else (error "not a tree"))))
```

7. Define the `tree-map!` function
Define a `tree-map!` function that applies a given function to each value in the tree, modifying the original tree.

```scheme
(define (tree-map! f tree)
  (cond
    ((node-nil? tree) "ah!")
    ((leaf? tree) (set-node-base-value! tree (f (node-base-value tree))))
    ((node? tree) (begin
                   (set-node-base-value! tree (f (node-base-value tree)))
                   (tree-map! f (node-left tree))
                   (tree-map! f (node-right tree))))
    (else (error "not a tree"))))
```

## Macros

### `++` increment operator

Define a macro for the `++` operator that is equivalent to this procedure:
```scheme
(define (++ x)
  (set! x (+ x 1))
  x)
```

Using `define-syntax` and `syntax-rules`: 
```scheme
(define-syntax ++
  (syntax-rules ()
    [(_ x)
     (+ 1 x)]))
```

Since there is just one syntax rule, we can use the short hand `define-syntax-rule`:
```scheme
(define-syntax-rule (++ x)
  (+ 1 x))
```


### Projecting arguments 

Create a variadic function called `proj-m` that takes an integer `n` and a list of arguments and returns the `n`th argument. The function should be zero-indexed, meaning the first argument is at index 0.

The function should be defined such that it fits the following usage:
```scheme
(proj-m 0 'a 'b 'c)  ; Should evaluate to 'a
(proj-m 1 'a 'b 'c)  ; Should evaluate to 'b
(proj-m 2 'a 'b 'c)  ; Should evaluate to 'c
```

A wrong solution could be:
```haskell
(define-syntax proj-m
  (syntax-rules ()
    ((_ n . params)
     (if (= n 0)
         (car params)
         (proj-m (- n 1) (cdr params))))
    ))
```
The problem is that macros don't evaluate expressions like `(= n 0)` or `(- n 1)` at runtime; they perform ***syntactic transformations***. As a result, the `if` expression in the macro is not evaluated as an `if` at compile time, leading to infinite recursion during macro expansion.

In macros we have to leverage *syntactic* recursion, that means using pattern matching instead of expression evaluation:
```scheme
(define-syntax proj-m
  (syntax-rules ()
    [(_ n v1)
     v1]
    [(_ n v1 v2 ...)
     (if (= n 0)
         v1  
         (proj-m (- n 1) v2 ...))]))
```
In this case the compiler will expand the macro until the first rule is expanded. The exact number of recursion steps does not depend on `n` but instead on the number of parameters passed.

### Function with type checking

Define a macro `define-with-types` that allows the definition of functions with type checking. The macro takes a function name `f`, a return type `tf`, a list of parameters with their types `(x1 : t1) ...`, and a list of expressions `e1 ...`. It defines a function that checks the types of its parameters and its return value.

Using the function with type checking:
```scheme
(define-with-types (add-to-char : integer (x : integer?) (y : char?))
  (+ x (char->integer y)))
  
(char->integer #\y) ; => 121
(add-to-char 1 #\y) ; => 122
(add-to-char 1 121) ; => error "bad input types"
```

Solution:
```scheme
(define-syntax define-with-types
  (syntax-rules (:)
    ((_ (f : tf (x1 : t1) ...) e1 ...)
     (define (f x1 ...)
       (if (and (t1 x1) ...)
           (let ((res (begin
                        e1 ...)))
             (if (tf res)
                 res
                 (error "bad return type")))
           (error "bad input types"))))))
```

>[!help]- Explanation
> The ellipsis `...` in Scheme macros is used to represent zero or more repetitions of the preceding form.
> In this macro, `(x1 : t1) ...` means that there can be zero or more `(x : t)` pairs, where `x` is a function argument and `t` is its expected type. The `...` after `(t1 x1)` means that `(t1 x1)` will be repeated for each `(x : t)` pair.
> Therefore, `(and (t1 x1) ...)` will expand to something like `(and (t1 x1) (t2 x2) ...)`, where `t1`, `t2`, etc. are the type-checking functions and `x1`, `x2`, etc. are the arguments to check. 
> 
> The `(e1 ...)` syntax is used to represent the body of the function in the macro. The `...` in `(e1 ...)` is an ellipsis, which indicates zero or more repetitions of the preceding form, allowing multiple expressions, not just a single one. If we used `body` and `(res body)`, it would mean that the function body can only contain a single expression, and that expression would be bound to `res`. This would be less flexible and wouldn't allow for functions with multiple expressions in their bodies.

### Block syntax

Write a Scheme macro `block` that takes a series of expressions to be evaluated in two different contexts. The first context and the second context are separated by the `then` keyword followed by a list of expressions. After the second context follows the `where` keyword which specifies variables and their values for each context.

For example, the provided `block` macro should work as follows:
```scheme
(block
 ((displayln (+ x y))
  (displayln (* x y))
  (displayln (* z z)))
 then
 ((displayln (+ x y))
  (displayln (* z x)))
 where (x <- 12 3)(y <- 8 7)(z <- 3 2))
; 20
; 96
; 9
; 10
; 6

(block
 ((displayln "one")
 (displayln "two"))
 then
 ((displayln "three")))
; one
; two
; three
```
The `block` macro should first evaluate the expressions in the first block with `x` as `12`, `y` as `8`, and `z` as `3`. Then, it should evaluate the expressions in the second block with `x` as `3`, `y` as `7`, and `z` as `2`.

Solution:
```scheme
(define-syntax block
  (syntax-rules (where then <-)
    ((_ (a ...) then (b ...))
     (begin
       a ...
       b ...))
    ((_ (a ...) then (b ...) where (v <- x y) ...)
     (begin
       (let ((v x) ...)
         a ...)
       (let ((v y) ...)
         b ...)))))
```


### Message dispatcher

Define a Scheme macro `define-dispatcher` that creates a lambda function to dispatch messages to different methods. The `methods:` keyword is followed by a list of method names, and the `parent:` keyword is followed by a parent function to call if no method matches the dispatched message.

For example, the provided `define-dispatcher` macro should work as follows:
```scheme
;; Define example methods and parent method
(define (m1 . pars)
  (begin
    (display "Method 1 called with parameters: ")
    (displayln pars)))

(define (m2 . pars)
  (begin
    (display "Method 2 called with parameters: ")
    (displayln pars)))

(define (pm msg . pars)
  (begin
    (display "Parent method called with message: ")
    (display msg)
    (display " and parameters: ")
    (displayln pars)))

;; Create the dispatcher
(define my-dispatcher
  (define-dispatcher methods: (m1 m2) parent: pm))

;; Test the dispatcher
(my-dispatcher 'm1 'a 'b) ; > "Method 1 called with parameters: (a b)"
(my-dispatcher 'm2 'c 'd) ; > "Method 2 called with parameters: (c d)"
(my-dispatcher 'unknown 'e 'f) ; > "Parent method called with message: unknown and parameters: (e f)"
```
`my-dispatcher` is a function that dispatches its arguments to `method1` or `method2` depending on the first argument (the message). If neither `method1` nor `method2` matches the message, it dispatches the arguments to `parent-method`.

Solution:
```scheme
(define-syntax define-dispatcher
  (syntax-rules (methods: parent:)
    ((_ methods: (method1 method2 ...) parent: parent-method)
     (lambda (msg . args)
       (cond ((eq? msg 'method1) (apply method1 args))
             ((eq? msg 'method2) (apply method2 args))
             ...
             (else (apply parent-method msg args)))))))
```

## Continuations

### Break and Continue

Write two Scheme functions `break-negative` and `continue-negative` that take a list of numbers as input. The `break-negative` function should display each number in the list until it encounters a negative number, at which point it should stop. The `continue-negative` function should display each non-negative number and skip any negative numbers. Use continuations to control the flow of execution.

For example, the provided `break-negative` function should work as follows:
```scheme
(break-negative '(1 2 3 -4 5 6)) ; displays 1, 2, 3 and then stops
```

And the `continue-negative` function:
```scheme
(continue-negative '(1 2 3 -4 5 6)) ; displays 1, 2, 3, 5, 6 skipping the negative number
```

Solution:
```scheme
(define (break-negative list)
  (call/cc (lambda (break)
             (let loop ((l (cdr list)) (h (car list)))
               (if (or (null? h) (< h 0))
                   (break)
                   ((display h)
                    (loop (cdr l) (car l))))))))

(define (continue-negative list)
  (let loop ((l (cdr list)) (h (car list)))
    (call/cc (lambda (continue)
               (if (or (null? h) (< h 0))
                   (continue)
                   (display h))))
    (unless (null? l)
      (loop (cdr l) (car l)))))
```

Alternative solution using `for-each`:
```scheme
(define (break-negative l)
  (call/cc (lambda (b)
             (for-each (lambda (x)
                         (if (< x 0)
                             (b 'end)
                             (displayln x)))
              l))))

(define (continue-negative l)
  (for-each (lambda (x)
              (call/cc (lambda (c)
                         (if (< x 0)
                             (c)
                             (displayln x)))))
            l))
```

### Non-local exits

Implement a mechanism a mechanism for non-local exits using continuations, to do that:
-  Use a global variable `*storage*` to store continuations
- Define a function `ret` that calls the most recent continuation with a given argument.
Finally, implement a macro `defun` that defines functions using continuations stored in `*storage*`:
1. uses the `call/cc` function to capture the current continuation and push it onto the `*storage*` stack
2. The body of the function is then evaluated, and the result is stored in `v`. 
3. After the function body is executed, the continuation is popped from the `*storage*` stack, and `v` is returned.

As an example, we define a function `g` that takes two numbers as arguments and returns the smaller number using the `ret` function and the `defun` macro:
```scheme
(define *storage* '()) ; continuations stack

(define (ret x)
  ((car *storage*) x))

(defun g (x y)
 (if (< x y)
	 (ret x)
	 y))

(g 2 3) ; returns 2
(g 3 2) ; returns 2
```
The function `g` returns the smaller of its two arguments. If `x` is less than `y`, it uses the `ret` function to return `x`. Otherwise, it returns `y`.

`defun` possible implementation:
```scheme
(define-syntax defun
  (syntax-rules ()
    ((_ f (p ...) b ...)
     (define (f p ...)
       (let ((v (call/cc (lambda (c)
                  (set! *storage* (cons c *storage*))
                  b ...))))
         (set! *storage* (cdr *storage*))
         v)))))
```

## Closures as Objects

### Person Object

Define a Scheme object `person` that has the following attributes and methods:
- Attributes: `name` and `age`
- Methods: `get-name`, `grow-older`, and `show`
The `get-name` method should return the person's name, the `grow-older` method should take an integer argument and increase the person's age by that amount, and the `show` method should display the person's name and age.
You should be able to create new `person` objects with the `new-person` constructor and call the methods on them.
Use closures to scope attributes and methods within the `person` object (instead of using a structure type).

Example usage:
```scheme
(define ada (new-person "Ada" 25))
(define bob (new-person "Bob" 25))
(ada 'grow-older 10) ; => 35
(bob 'get-name) ; => "Bob"
(ada 'show) ; => "Name: Ada\nAge: 35"
(bob 'show) ; => "Name: Bob\nAge: 25"
```

Solution:
```scheme
(define (new-person
         initial-name ;; initial values / constructor
         initial-age)
  ;; attributes
  (let ([name initial-name]
        [age initial-age])
  ;; methods
    (define (get-name) ; getter for public attribute
      name)
    (define (grow-older years) ; a method to change age (and return it)
      (set! age (+ age years))
      age)
    (define (show) ; another method
      (display "Name: ")(displayln name)
      (display "Age: ")(displayln age))
    ;; dispatcher (to handle calls to methods)
    (λ (message . args)
      (apply (case message
               [(get-name) get-name]
               [(grow-older) grow-older]
               [(show) show]
               [else (error "unknown method")])
             args))))
```

### Inheritance from Person

Define a Scheme object `superhero` that inherits from the `person` object defined in the previous exercise. The `superhero` object should have the following additional attributes and methods:
- Attributes: `power`
- Methods: `use-power`
The `use-power` method should display the superhero's name and power when called.
The `superhero` object should inherit the `name`, `age`, `get-name`, `grow-older`, and `show` methods from the `person` object.

Example:
```scheme
(define superman (new-superhero "Clark Kent" 32 "Flight"))
(superman 'use-power) ; => "Clark Kent uses Flight!"
(superman 'grow-older 10) ; => 42
(superman 'show) ; => "Name: Clark Kent\nAge: 42\nPower: Flight"
```

Solution:
```scheme
;; Inheritance
(define (new-superhero name age init-power)
  (let ([parent (new-person name age)] ; inherits attrs/methods
        [power init-power])
    (define (use-power)
      (display name)(display " uses ")(display power)(displayln "!"))
    (define (show)
      (parent 'show)
      (display "Power: ")(displayln power))

    (λ (message . args)
      (case message
        [(use-power) (apply use-power args)]
        [(show) (apply show args)] ; overrides Person.show
        [else (apply parent (cons message args))]))))
```
