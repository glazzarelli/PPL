## Scheme

**#lang racket** directive at the beginning of the file (.rkt) to choose the Scheme dialect.\
Scheme is formed by s-expressions, the language itself can be written as a syntax tree.\
Scheme, like Lisp, is *homoiconic*, i.e. there's no distinction between code and data.

### Basic types, binding and control flow

**Basic types:**
- Booleans: `#t`, `#f`
- Numbers: `132897132989731289713`, `1.23e+33`, `23/169`, `12+4i`
- Characters: `#\a`, `#\Z`
- Symbols: `a-symbol`, `another-symbol?`, `indeed!`
- Vectors: `#(1 2 3 4)`
- Strings: `"this is a string"`
- Pairs and Lists: `(1 2 #\a)`, `(a . b)`, `()` empty list

**Evaluation** of an expression produces a value, the evaluation of an expression (e1 e2 e3 . . .) is based on the evaluation of e1, which identifies an operation f (e.g. is the name of a procedure).
The other sub-expressions (i.e. ei, i > 1) are evaluated, and their values are passed to f.
The evaluation order of ei, i ≥ 1 is unspecified, e.g. e4 could be evaluated before e2.

**lambda** is an unnamed procedure, definition and usage:\
`((lambda (arguments) function) parameters)`
`((lambda (x y) (+ (* x x) (* y y))) 2 3) ; =>13`\
Procedures are values, hence are first class objects.

**let** is used for binding variables:\
`(let ((x 2)(y 3)))`\
Scoping rules are static, the following examples displays `1` : 
```
(let ((a 1))
   (let ((f (lambda ()
	(display a))))
    (let ((a 2))
         (f)) ))
```
let binds variables "in parallel", there's no need to use a `tmp` variable to swap two variables:
```
(let ((x 1) 
      (y 2))
      (let ((x y) ; swap x and y
            (y x)) 
	x)) ; =>2
```
**let\*** is a "sequential" variant of let, with it, x is bound before y.

**if, when, unless**
```
(if (predicate) consequent (alternative))
(when (condition) (then-part))
(unless (condition) (else-part)) 
```

We can use the **begin** construct for writing a block of procedural code:
```
(begin
  (op_1 ...)
  (op_2 ...)
  ...
  (op_n ...))
```
Every *op_i* is evaluated in order, and the value of the begin block is the value
obtained by the last expression.

**named let** is used to create general loops in a "non-idiomatic" way:
```
(let ((x 0))
  (let label () ; why an empty list?
    (when (< x 10)
	(display x)
	(newline)
	(set! x (+ 1 x))
	(label)))) ; go-to label
```
The empty list can be used for variables that are used in the loop, indeed the more idiomatic way of doing the same:
```
(let label ((x 0))
  (when (< x 10)
    (display x)
    (newline)
    (label (+ x 1)))) ; x++
```
Like with begin, the value is the one obtained by the last expression

**quote** used to prevent evaluation `(quote <expr>)` or `'<expr>`.\
**quasiquote** ``    ` `` and **unquote** `,` are used for partial evaluation.
```
'(1 2 3)        ; = (quote (1 2 3)) => (1 2 3)
`(1 ,(+ 1 1) 3) ; = (quasiquote
                ;      (1 (unquote (+ 1 1)) 3))
                ; => (1 2 3)
```
***Note:*** there are different type of "quotes":\
``    ` `` adds a quasiquote\
`'` adds a quote, it used also for list, can be read as `list`\
`,` adds unquote\
`‘` and `’` are a mistery, but the professor uses them as quote and quasiquote in the slides

Variables created by let are local. To create top-level bindings there is
**define** `(define <name> <what>)`.
```
(define x 12)		    ; integer variable
(define y #(1 2 3))	    ; mutable vector
(define z '(1 2 3))	    ; mutable list
(define cube (lambda (x) (* x x x)))
(cube 3) ; => 27	      procedure cube define with lambda, alternatively:
(define (square x) (* x x)) ; procedure defined with name and argument
(square 5) ; => 25  	
```
*Define* can be also used instead of *let*  in procedures.\

**set!** is for assignment:
```
(begin
    (define x 23)
    (set! x 42)
    x) ; => 42
```
***Note:*** in general, procedures with side effects have a trailing bang `!` character.

### Equivalence predicates
A predicate is a procedure that returns a Boolean, its name usually ends with `?`.\
`=` is used only for numbers.\
`eq?` tests if two objects are the same (good for symbols).\
`eqv?` like *eq?*, but checks also numbers.\
`equal?` predicate is `#t` if and only if the (possibly infinite) unfoldings of its arguments
into regular trees are equal as ordered trees.

`(eq? ’casa ’casa) ; #t`\
`(eq? "casa" (string-append "ca" "sa")) ; #false`\
`(eq? 2 2) ; is unspecified`\
`(equal? (make-vector 5 ’a)(make-vector 5 ’a)) ; is true`

**case** and **cond** syntax rules.
```
(case (car '(c d))
          ((a e i o u) 'vowel) ; vowel cases grouped together
          ((w y) 'semivowel)
          (else 'consonant))) ; => 'consonant
```
```
(cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal))) ; => 'equal
```
***Note:*** they are all *symbols*, neither strings nor characters; the predicate used in case is *eqv?*.

Other predicates:\
`(null? list)` check if list is null, if not, return false
<!-- (add examples) -->

### Lists and vectors

**Lists** are memorized as concatenated pairs, a pair (written (x . y), also called a *cons* node) consists of two parts: (*car* . *cdr*); a list `'(1 2 3)` is stored like this (1 . (2 . (3 . ()))), where car=1 and cdr=(2 . (3 . ())).\
`()` is the empty list also called nil in Lisp.

_Examples_:\
`()` reads equal to `(list)`\
`(1 2 3)` reads equal to `(list 1 2 3)`\
`{1 2 3}` reads equal to `(list 1 2 3)`\
`[1 2 3]` reads equal to `(list 1 2 3)`\
`(1 (2) 3)` reads equal to `(list 1 (list 2) 3)`\
`(1 . 3)` reads equal to `(cons 1 3)`\
`(1 . (3))` reads equal to `(list 1 3)`\
`(1 '(3))` reads equal to `(list 1 3)`\
`(1 . 2 . 3)` reads equal to `(list 2 1 3)`\
`#(1 apple 3)` reads equal to `(vector 1 'apple 3)`\
`#3("apple" "banana")` reads equal to `(vector "apple" "banana" "banana")`\
`#3()` reads equal to `(vector 0 0 0)`

**car** and **cdr** can be merged together: \
`(car '(1 2 3)) ; => 1`\
`(list (car '(1 2 3))) ; => '(1)`\
`(cdr '(1 2 3)) ; => '(2 3)`\
`(cadr '(1 2 3)) ; car of a cdr => 2`

**append** to append a list to another one `(append '(5 6) '(3 2 4)) ; => '(5 6 3 2 4)`\
**take** to get the prefix of a list `(take '(4 2 5 7 3) 3) ; '(4 2 5)`\
**member** to check if a list contains a value `(member 2 '(1 2 3)) ; => '(2 3)`.\
**apply** can be used to apply a procedure to a list of elements `(apply + '(1 2 3 4)) ; => 10`.\
**cons** to build a pair `(cons 1 2)` is `(1 . 2)`, `(cons 1 '(2))` is `(1 2)`, which is a list.\
**for-each**, similar to apply, it applies a procedure recursively using each element of the list as argument.
`(for-each (lambda (x) (display (* 2 x))) '(1 2 3 4)) ; => 2468`

**vector-length** return the size of the vector.\
**vector-ref** gets the referenced value by index i in vector `(vector-ref #(58 34 12 43) 3) ; => 43`.\
**vector-set!** set the element indexed by i to val `(vector-set! vect i val) ; vect[i] = val`.\
***Note:*** literal constants seen in the last examples are immutable, this means that procedures as *vector-set!* can be applied only to a vector (or list) initialized using *let* (local) or *define* (global).

It is possible to define procedures with a **variable number of arguments**, passing them as a cons node:
```
(define (x . y) y)
(x 1 3 5 7) ; => '(1 3 5 7)
```
In this case we defined a function named *x*, which is also a cons node, and all the parameters are stored in y. In the general case we pass a list *L* as parameter:
```
(define (minimum L)
 (if (null? (cdr L)) (car L) 
  (min (car L) (minimum(cdr L)))))

(define (minimum L) ; without binary op min
  (let ((x (car L)) (xs (cdr L)))
    (if (null? xs) (minimum
      (cons (if (< x (car xs)) x (car xs))
            (cdr xs ))))))
```
**min** and **max** are binary procedures.

### Tail recursion
Every Scheme implementation is required to be properly tail recursive. A procedure is called tail recursive if its recursive call is "at the tail", i.e., is
the last operation performed, examples of both:
```
(define (factorial n) ; not tail recursive
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
	  
(define (fact n)  ; tail recursive
  (define (fact-tail x accum) ; local proc
    (if (= x 0) accum
        (fact-tail (- x 1) (* x accum ))))
        (fact-tail n 1))	  
```
The not tail recursive implementation will expand first all the factors of the factorial (and return addresses, etc.) on the stack, this leads to use of unnecessary use of memory.\
The tail recursive version makes use of a **local procedure** *fact-tail* defined with two arguments, a number *x* and an **accumulator** *accum*, and return itself with *x-1* and *accum \* x*, if *x* is greater than 0, else it returns the accumulator. Then, at the end of the procedure, *fact* calls *fact-tail* once with parameter *n*. The procedure *fact-tail* won't do nested calls but will return immediately, calling itself again, using the partial result stored in the accumulator.\
Tail recursive procedures can be optimized to avoid stack consumption, indeed, the previous tail call is translated in the following low-level code:
```
(define (fact-low-level n)
  (define x n)
  (define accum 1)
  (let loop () ; see this as the "loop" label
    (if (= x 0)
      accum
      (begin
        (set! accum (* x accum ))
        (set! x (- x 1))
        (loop )))))) ; jump to "loop"
```

### Evaluation strategy
*Call by object sharing* (like in Java): objects are allocated on the heap and references to them are passed by value. It is also often called *call by value*, because objects are evaluated before the call, and such values are copied into the activation record. 
```
(define (test-setting-local d)
  (set! d "Local") ; setting the local d
  (display d)(newline))

(define ob "Global")
(test-setting-local ob) ; => Local
(display ob) ; => Global
```
The copied value is not the object itself, which remains in the heap, but a reference to the object; this means that, if the object is mutable, the procedure may exhibit side effects on it:
```
(define (set-my-mutable d)
  (vector-set! d 1 "done")
  (display d))
(define ob1 (vector 1 2 3)) ; i.e. #(1 2 3)
(set-my-mutable ob1) ; => #(1 done 3)
(display ob1) ; => #(1 done 3)
```

###
