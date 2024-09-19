## Scheme

### Intro 

%% add disclaimer about gpt4, slides form polimi course and other sources %%
**Scheme** and **Racket** are both dialects of the Lisp programming language, but they have significant differences in their design philosophies, features, and use cases.

Scheme is designed to be a minimalistic language, with a small core language and few built-in features. 

Scheme is not a subset of Racket, but rather, Racket started as an implementation of the Scheme programming language and has since evolved to become a separate language with its own unique features.

The relationship between Scheme and Racket can be thought of more as a parent-child relationship. Racket was initially developed as a platform for Scheme systems, but it has since grown into its own language with features that extend beyond those found in Scheme: Racket includes many built-in features and libraries, including a powerful macro system, support for object-oriented programming, and extensive libraries for tasks such as web programming and GUI development.

Racket It also includes a built-in IDE,  *[DrRacket](https://racket-lang.org/)* which we will use to test our *Racket* code, though most of the time it will be also valid scheme code.

In Scheme, a simple function to add two numbers might look like this:
```scheme
(define (add x y)   (+ x y))
```

In Racket, the same function would be written like this:
```scheme
#lang racket
(define (add x y)   (+ x y))
```
The **`#lang racket`** directive at the beginning of the file (`.rkt`) tells DrRacket to choose the Scheme dialect.  

Key aspects of Scheme to keep in mind:
- Scheme is formed by s-expressions, the language itself can be written as a syntax tree.  
- Scheme, like Lisp, is *homoiconic*, i.e., there's no distinction between code and data.

### Basic types, binding and control flow

**Basic types:**
- Booleans: `#t`, `#f`
- Numbers: `132897132989731289713`, `1.23e+33`, `23/169`, `12+4i`
- Characters: `#\a`, `#\Z`
- Symbols: `a-symbol`, `another-symbol?`, `indeed!`
- Vectors: `#(1 2 3 4)`
- Strings: `"this is a string"`
- Pairs and Lists: `(1 2 #\a)`, `(a . b)`, `()` empty list

**Evaluation** of an expression produces a value, the evaluation of an expression ($e_1$ $e_2$ $e_3$ . . .) is based on the evaluation of $e_1$, which identifies an operation $f$ (e.g. is the name of a procedure).
The other sub-expressions (i.e. $e_i,i > 1$ ) are evaluated, and their values are passed to $f$.
The evaluation order of $e_i, i ≥ 1$ is unspecified, e.g. $e_4$ could be evaluated before $e_2$.

**`lambda`** is an unnamed procedure, definition and usage:  
```scheme
((lambda (arguments) function) parameters)
((lambda (x y) (+ (* x x) (* y y))) 2 3) ; =>13
```

Procedures are values, hence are first class objects.

**`let`** is used for binding variables:  
```scheme
(let ((x 2)(y 3)))
```

Scoping rules are *static*, the following examples displays `1` : 
```scheme
(let ((a 1))
   (let ((f (lambda ()
	(display a))))
    (let ((a 2))
         (f)) ))
```

>[!help]- Explanation:
> 1. In the outer `let` expression, `a` is bound to `1`.
> 2. The middle `let` expression defines a function `f` that displays the value of `a` when called. Here, `f` is a lambda function, which is an anonymous function. The `lambda` function here takes no arguments and displays the value of `a`.
> 3. The inner `let` expression re-binds `a` to `2` and then calls the function `f`.
>
> When `f` is called, it displays the value of `a`. However, because of the way scoping works in Scheme, it displays the value of `a` from the scope where `f` was defined, not the value of `a` in the current scope.
> 
> So, even though `a` is set to `2` in the inner `let` expression, when `f` is called, it displays `1`, because that was the value of `a` in the scope where `f` was defined. This is a demonstration of the concept of a "closure" in computer science, where a function retains access to the variables from its parent scope.
> 
> In summary, this code demonstrates how variable scope and closures work in Scheme.

**`let`** binds variables "in parallel", there's no need to use a `tmp` variable to swap two variables:
```scheme
(let ((x 1) 
      (y 2))
      (let ((x y) ; swap x and y
            (y x)) 
	x)) ; =>2
```
**`let*`** is a "sequential" variant of let, with it, x is bound before y.

**`if`, `when`, `unless`**
```scheme
(if (predicate) consequent (alternative))

(when (condition) (then-part))

(unless (condition) (else-part)) ;which is equal to:
(when (not (condition)) (then-part))
```

We can use the **`begin`** construct for writing a block of procedural code:
```scheme
(begin
  (op_1 ...)
  (op_2 ...)
  ...
  (op_n ...))
```
Every *op_i* is evaluated in order, and the value of the begin block is the value
obtained by the last expression.

**named `let`** is used to create general loops in a "non-idiomatic" way:
```scheme
(let ((x 0))
  (let label () ; why an empty list?
    (when (< x 10)
      (display x)
      (newline)
      (set! x (+ 1 x))
      (label)))) ; go-to label
```

>[!help]- Explanation
`(let label () ...)` is a named `let` block, also known as a `letrec` block. It defines a function named `label` that takes no arguments (hence the empty list `()`). This function is recursive, meaning it can call itself, and is scoped within the outer `let` block, so it has access to the variable `x`.
The empty list can be used for variables that are used in the loop, indeed the more idiomatic way of doing the same:
> 
> ```scheme
> (let label ((x 0))
>   (when (< x 10)
>     (display x)
>     (newline)
>     (label (+ x 1)))) ; x++
> ```
> Like with `begin`, the value returned by the `let` form is the value of the last expression evaluated, which, in this case, is the recursive call to the `label` function.
> 
> Multiple arguments in a named `let` is the same as before, we just call the function `label` with all its argument:
> ```scheme
> (let label ((x 0) (y 20)) ; Initial values for x and y
>   (when (< x 10) ; Loop condition
>     (display "x: ") ; Display x label
>     (display x) ; Display x value
>     (display ", y: ") ; Display y label
>     (display y) ; Display y value
>     (newline) ; New line for each iteration
>     (label (+ x 1) (- y 2)))) ; Recursive call with updated values
> ```

**`quote`** \ `'` used to prevent evaluation `(quote <expr>)` or `'<expr>`.  
**`quasiquote`** `` ` `` and **unquote** `,` are used for partial evaluation.
```scheme
'(1 2 3)        ; = (quote (1 2 3)) => (1 2 3)
`(1 ,(+ 1 1) 3) ; = (quasiquote
                ;      (1 (unquote (+ 1 1)) 3))
                ; => (1 2 3)
```

>[!note]- Which quote do what?
There are different type of "quotes":  
`` ` `` adds a `quasiquote`  
`'` adds a `quote`, it used also for list, can be read as `list`  
`,` adds `unquote`  
`‘` and `’` are a mistery, but the professor uses them as `quote` and `quasiquote` in the slides

Variables created by `let` are local. To create top-level bindings there is
**`define`**, syntax: `(define <name> <what>)`.
```scheme
(define x 12)		    ; integer variable
(define y #(1 2 3))	    ; mutable vector
(define z '(1 2 3))	    ; mutable list
(define cube (lambda (x) (* x x x)))
(cube 3) ; => 27	      procedure cube define with lambda, alternatively:
(define (square x) (* x x)) ; procedure defined with name and argument
(square 5) ; => 25  	
```
`define` can be also used instead of `let`  in procedures.

**`set!`** is for assignment:
```scheme
(begin
    (define x 23)
    (set! x 42)
    x) ; => 42
```
In general, procedures with side effects have a trailing bang `!` character.

### Equivalence predicates

A predicate is a procedure that returns a Boolean, its name usually ends with `?`.  
`=` is used only for numbers.  
`eq?` tests if two objects are the same (good for symbols).  
`eqv?` like *`eq?`*, but checks also numbers.
`equal?` predicate is `#t` if and only if the (possibly infinite) unfoldings of its arguments into regular trees are equal as ordered trees.
`(null? list)` check if list is null, if not, return false

```scheme
(eq? 'casa 'casa) ; #t 
(eq? "casa" (string-append "ca" "sa")) ; #false
(eq? 2 2) ; is unspecified
(equal? (make-vector 5 ’a)(make-vector 5 ’a)) ; is true
```

The **`case`** expression provides a way to test a single expression against multiple conditions and return a corresponding value.
It acts like a switch in c language, the predicate used in `case` is `eqv?`.

```scheme
(case (car '(c d))
          ((a e i o u) 'vowel) ; vowel cases grouped together
          ((w y) 'semivowel)
          (else 'consonant)) ; => 'consonant
```

>[!help]- Explanation
>1. The `car` function is called on the list `'(c d)`, which returns the first element of the list, 'c' in this case.
>2. The `case` keyword indicates a multi-branch conditional. The value returned by `(car '(c d))` is compared against the values in the different clauses. Each clause is a list where the first element is a list of values to match against, and the second element is the result to return if there's a match.
>3. In this specific case, the character 'c' does not match any of the values in the `vowel` or `semivowel` clauses. Therefore, the `else` clause is executed, and the symbol 'consonant' is returned.
>
> In Scheme, characters are represented using a syntax like `#\a`, `#\b`, etc. Therefore, the `#\` prefix is used to denote a character. This is contrasted from the list `'(a b c)` where `a`, `b`, and `c` are symbols, not characters.

The `cond` function works by evaluating a series of conditions from top to bottom. When it finds the first condition that evaluates to `#t` (true), it executes the corresponding expression and returns its value, then stops further processing. If no conditions are met, it evaluates the `else` clause if present.

```scheme
(cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal)) ; => 'equal
```

>[!note]- What is a symbol?
>In the context of Scheme and other Lisp-like languages, a symbol is a type of data that's used to represent identifiers or names in programs. A symbol is a unique identifier that has a name (which is a string of characters). However, unlike strings, symbols with the same name are identical in the sense that they occupy the same memory space [gnu.org](https://www.gnu.org/software/guile/manual/html_node/Symbols.html).

```scheme
(define a 1)
(define b 2)
(define c 3)
 
(a b c)  ; => error
'(a b c) ; => (a b c)
```

In the first case, Scheme tries to apply `a` (which is the number `1`) as a function to the arguments `b` and `c`, which results in an error because `1` is not a function. In the second case, Scheme treats `(a b c)` as a list of symbols, so it returns the list as is [courses.cs.washington.edu](https://courses.cs.washington.edu/courses/cse341/04wi/lectures/14-scheme-quote.html), [gnu.org](https://www.gnu.org/software/guile/manual/html_node/Symbols.html).

### Lists

**Lists** are memorized as concatenated pairs.
A pair, which is written (x . y) and it is also called a *cons* node, consists of two parts: (*car* . *cdr*). The list `'(1 2 3)` is stored like this (1 . (2 . (3 . ()))), where car=1 and cdr=(2 . (3 . ())).  
`'()` is the empty list also called *nil* in Lisp.

> [!Example]
`()` reads equal to `(list)`
`(1 2 3)` reads equal to `(list 1 2 3)`  
`{1 2 3}` reads equal to `(list 1 2 3)`  
`[1 2 3]` reads equal to `(list 1 2 3)`  
`(1 (2) 3)` reads equal to `(list 1 (list 2) 3)`  
`(1 . 3)` reads equal to `(cons 1 3)`  
`(1 . (3))` reads equal to `(list 1 3)`  
`(1 '(3))` reads equal to `(list 1 3)`  
`(1 . 2 . 3)` reads equal to `(list 2 1 3)`  

**`car`** and **`cdr`** can be merged together:  
`(car '(1 2 3)) ; => 1`  
`(list (car '(1 2 3))) ; => '(1)`  
`(cdr '(1 2 3)) ; => '(2 3)`  
`(cadr '(1 2 3)) ; c-ad-r = car of a cdr => 2`

**`cons`** builds a pair `(cons 1 2)` is `'(1 . 2)`, `(cons 1 '(2))` is `'(1 2)`, which is a list: lists are stored as nested cons nodes where the head *car* is an element and the body with head *cdr* is a list. 

**`append`** to append a list to another one `(append '(5 6) '(3 2 4)) ; => '(5 6 3 2 4)`    

**`take`** to get the prefix of a list `(take '(4 2 5 7 3) 3) ; '(4 2 5)` 

**`member`** to check if a list contains a value `(member 2 '(1 2 3)) ; => '(2 3)`.  

**`apply`** can be used to apply a procedure to a list of elements.
```scheme
(apply + '(1 2 3 4)) ; => 10`.  
```

**`for-each`**, similar to apply, it applies a procedure recursively using each element of the list as argument.
```scheme
(for-each (lambda (x) (display (* 2 x))) '(1 2 3 4)) ; => 2468
```

It is possible to define procedures with a ***variable number of arguments***, passing them as a cons node:
```scheme
(define (x . y) y)
(x 1 3 5 7) ; => '(1 3 5 7)
```
In this case we defined a function named *x*, which is also a cons node, and all the parameters are stored in y. In the general case we pass a list *L* as parameter:
```scheme
(define (minimum L)
 (if (null? (cdr L)) (car L) 
  (min (car L) (minimum(cdr L)))))

(define (minimum L) ; without binary op min
  (let ((x (car L)) (xs (cdr L)))
    (unless (null? xs) (minimum
      (cons (if (< x (car xs)) x (car xs))
            (cdr xs)) ))))
```
**`min`** and **`max`** are binary scheme procedures.

>[!note]- `cons` vs `list`
>In Racket, a list and a cons cell (or node) are both fundamental data structures, but they have some differences in terms of their structure and usage.
> A list in Racket is a sequence of values. When you create a list with two items, you're creating a sequence that contains exactly those two items. A list of two items can be created using the `list` function, like so:
> 
> ```scheme
> (define my-list (list 'a 'b))
> ; '(a . (b . ())) how is it saved
> ```
> 
> On the other hand, a cons node or a cons cell is a data structure that holds two values or references to values. It's called a "cons node" because it's created using the `cons` function. A cons node isn't necessarily a list. For example, the expression `(cons 'a 'b)` creates a cons cell holding 'a' and 'b', but it's not considered a list because its second element isn't a list.
> 
> ```scheme
> (define my-cons (cons 'a 'b))
> ; '(a . b) how is it saved
> ```
> 
> To convert a cons node to a list, you can use the `cons` function with the second argument being a list. For example, `(cons 'a '('b))` or creates a list of two elements 'a' and 'b'. Here, 'a' is the first element of the list, and the list containing 'b' is the rest of the list.
> 
> ```scheme
> (define my-list-from-cons (cons 'a (list 'b)))
> ```
> 
> To convert a list to a cons node, you can use the `car` and `cdr` functions. `car` returns the first element of the list, and `cdr` returns a list of the remaining elements. So, `(cons (car my-list) (cadr my-list))` creates a cons cell from a list.
> 
> ```scheme
> (define my-cons-from-list (cons (car my-list) (cadr my-list)))
> ```
> 
> In summary, while both lists and cons cells can hold multiple values, they are used in different ways. Lists are for sequences of values, while cons cells can be used to build more complex data structures. 

### Vectors

Vectors are heterogeneous structures whose elements are indexed by integers. They are similar to arrays in other languages. Accessing an arbitrary element in a list requires a linear traversal, while arbitrary vector elements can be accessed in constant time.

> [!Example]
`#(1 apple 3)` reads equal to `(vector 1 'apple 3)`  
`#3("apple" "banana")` reads equal to `(vector "apple" "banana" "banana")`  
`#3()` reads equal to `(vector 0 0 0)`

**`vector-length`** return the size of the vector.

**`vector-ref`** gets the referenced value by index $i$ in vector `(vector-ref #(58 34 12 43) 3) ; => 43`.

**`vector-set!`** set the element indexed by $i$ to val `(vector-set! vect i val) ; vect[i] = val`.

> [!note]
> In Scheme, literal constants are immutable, meaning their values cannot be changed once they are defined. They are treated as self-evaluating expressions, which means they evaluate to themselves. In Scheme, literal constants can include numbers, characters, strings, and boolean values. Here are some examples of literal constants in Scheme:
> 
> - Number literals: `1`, `3.14`, `-5`
> - Character literals: `a`, `b`, `c`
> - String literals: `"Hello"`, `"World"`
> - Boolean literals: `#t`(true), `#f` (false)
> 
> Literal constants seen in the last examples are immutable, this means that procedures as `vector-set!` can be applied only to a vector (or a list) initialized using `let` (local) or `define` (global).

### Tail recursion

Every Scheme implementation is required to be properly tail recursive. A procedure is called *tail recursive* if its recursive call is "at the tail", i.e., is the last operation performed, examples of both:
```scheme
(define (factorial n) ; not tail recursive
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
	  
(define (fact n)  ; tail recursive
  (define (fact-tail x accum) ; define local procedure
    (if (= x 0) accum
        (fact-tail (- x 1) (* x accum ))))
        (fact-tail n 1))      ; call local procedure
```

>[!help] Explanation
>The not tail recursive implementation will expand first all the factors of the factorial (and return addresses, etc.) on the stack, this leads to use of unnecessary use of memory.
> The tail recursive version makes use of a **local procedure** *fact-tail* defined with two arguments a number `x` and an **accumulator** `accum`.
> If `x` is zero, it directly returns the accumulated value, otherwise, it makes a recursive call to `fact-tail`, decrementing `x` by `1` and multiplying the accumulator by `x`.
> `fact` calls `fact-tail` once with parameter `n`. The procedure `fact-tail` won't do nested calls but will return immediately, calling itself again, using the partial result stored in the accumulator.
> The recursive call is the ***last operation*** in the function, making it a tail-recursive function, meanwhile in the non tail-recursive version the last operation is `*`.

Tail recursive procedures can be optimized to avoid stack consumption, indeed, the previous tail call is translated in the following low-level code:
```scheme
(define (fact-low-level n)
  (define x n)
  (define accum 1)
  (let loop () ; see this as the "loop" label
    (if (= x 0)
      accum
      (begin
        (set! accum (* x accum ))
        (set! x (- x 1))
        (loop)))))) ; jump to "loop"
```

### Evaluation strategy

*Call by object sharing* (like in Java): objects are allocated on the heap and references to them are passed by value. It is also often called *call by value*, because objects are evaluated before the call, and such values are copied into the activation record. 
```scheme
(define (test-setting-local d)
  (set! d "Local") ; setting the local d
  (display d)(newline))

(define ob "Global")
(test-setting-local ob) ; => Local
(display ob) ; => Global
```
The copied value is not the object itself, which remains in the heap, but a reference to the object; this means that, if the object is mutable, the procedure may exhibit side effects on it:
```scheme
(define (set-my-mutable d)
  (vector-set! d 1 "done")
  (display d))
  
(define ob1 (vector 1 2 3)) ; i.e. #(1 2 3)
(set-my-mutable ob1) ; => #(1 done 3)
(display ob1) ; => #(1 done 3)
```

### Structured types

It is possible to define new types, through **`struct`**, like in C but with some differences:
```scheme
(struct being (
  name            ; name is immutable
  (age #:mutable) ; flag for mutability
))
```
A number of related procedures are automatically created, e.g., the **constructor** `being` and a **predicate** to check if an object is of this type: `being?` in this case. 

Also **accessors** (and setters for mutable fields) are created:
```scheme
(define (being-show x)
  (display (being-name x))
  (display " (")
  (display (being-age x))
  (display ")"))

(define (say-hello x)
  (if (being? x) ; check if it is a being
    (begin
      (being-show x)
      (display ": my regards.")
      (newline))
    (error "not a being" x)))
```

```scheme
(define james (being "James" 58))
(say-hello james) ; => James (58): my regards.
(set-being-age! james 60) ; a setter
(say-hello james) ; => James (60): my regards.
```

**Structs** can inherit, but not override:
```scheme
(struct may-being
    being ; being is the father
    ((alive? #:mutable)) ; to be or not to be
)

(define (kill! x)
  (if (may-being? x)
    (set-may-being-alive?! x #f)
    (error "not a may-being" x)))
	
(define (try-to-say-hello x)
  (if (and
       (may-being? x)
       (not (may-being-alive? x)))
      (begin
        (display "I hear only silence.")
        (newline ))
      (say-hello x)))

(define john (may-being "John" 77 #t))
(kill! john)
(say-hello john) ; John (77): my regards.
(try-to-say-hello john) ; I hear only silence.
```
The main difference is in methods vs procedures: procedures are external, so with inheritance we cannot redefine/override them: still a `may-being` behaves like a `being`. We have to define a new procedure (i.e. `try-to-say-hello`), to cover the role of `say-hello` for a `may-being`.

>[!note] Why begin?
> `begin` is a special form (a kind of function) that can take any number of expressions and execute them in sequence, if we remove it the outer parentheses will try to apply the result of `(display "Hello, I'm ")` as a function, which is not possible because `display` returns a void value that cannot be used as a function.

### Closures

A *closure* is a function together with a referencing environment for the non-local variables of that function, i.e. a function object that "closes" over its visible variables.
```scheme
(define (make-adder n)
  (lambda (x)
    (+ x n)))

(define x 5)
(define add5 (make-adder x))
(define sub5 (make-adder -5))
(= (add5 x) (sub5 15)) ; => #t
x ; => 5 
```
`make-adder` is defined with a fixed environment variable *n* that remains inside the function itself thanks to static scoping, then each time the function is called it applies the procedure to the local `x` variable passed as argument.

A closure can be used as an **iterator**:
```scheme
(define (iter-vector vec)
  (let ((cur 0)
        (top (vector-length vec)))
    (lambda ()
      (if (= cur top)
          '<<end>>
          (let ((value (vector-ref vec cur)))
            (set! cur (+ cur 1)) value)))))

(define i (iter-vector #(1 2)))

(i) ; => 1
(i) ; => 2
(i) ; => ’<<end >>
```

>[!help]- Explanation
> 1. The `iter-vector` function is defined to take one argument, `vec`, which should be a vector. It uses a `let` expression to initialize two local variables: `cur` and `top`. `cur` is initially set to 0 and represents the current position in the vector. `top` is set to the length of the vector, obtained by calling the `vector-length` function on `vec`.
> 2. The `let` expression then returns a `lambda` function that takes no arguments. This returned function is the iterator. When this function is called, it checks if `cur` is equal to `top`, meaning it checks whether all elements in the vector have been iterated over. If they have, it returns the symbol `<<end>>`.
> 3. If `cur` is not equal to `top`, the function enters another `let` expression. This expression retrieves the vector element at the current position by calling `vector-ref` with `vec` and `cur` as arguments. The resulting value is bound to the local variable `value`.
> 4. The `cur` variable is then incremented by 1 using the `set!` procedure. This moves the iterator to the next position in the vector.
> 5. Finally, the `value` is returned. This is the current element of the vector.
> 
> The second part of the code creates an instance of this iterator for the vector `#(1 2)` and assigns it to the variable `i`. When `i` is called as a function, it returns the next element from the vector. After all elements have been returned, it starts returning `<<end>>`.

### Higher order functions

**`map`** takes a *procedure* and one or more lists , the procedure must have an arity equal to the number of list passed, if more list are passed they should have the same length:
```scheme
(map (lambda (x) (+ 1 x)) '(0 1 2)) ; => '(1 2 3)
(map (lambda (x) (+ 1 x)) '(0 1 2) '(9 4 10)) ; => error = map: argument mismatch
(map (lambda (x y) (* y x)) '(0 1 2) '(9 4 10)) ; => '(0 4 20)
```

>[!note]- `map` vs `for-each`
>In Racket, both `for-each` and `map` are procedures used to apply a function to each element of a list. The main difference lies in their return values and how they handle the results.
> 
> The `for-each` procedure applies a function to every element in a list but does not collect the results. Instead, it discards them, and its return value is unspecified. This makes `for-each` best suited for operations where the side effects are important, such as printing or updating global variables.
> 
> Here's an example of `for-each` usage:
> 
> ```scheme
> (define (print-square x) 
>   (display (* x x)))
> (for-each print-square '(1 2 3 4 5)) ; => 1491625
> ```
> 
> In contrast, the `map` procedure also applies a function to every element in a list, but it collects the results and returns a new list made up of these results. This makes `map` useful when you want to transform a list into a new list.
> 
> Here's an example of `map` usage:
> 
> ```scheme
> (define (square x) (* x x))
> (map square '(1 2 3 4 5)) ; => '(1 4 9 16 25)
> ```
> 
> In summary, while both `for-each` and `map` apply a function to each element of a list, `for-each` is used for its side effects and discards the results, while `map` collects the results into a new list and returns it.

**`filter`** takes a *predicate* and a list and return a filtered list:
```scheme
(filter (lambda (x) (> x 0)) '(10 -11 0)) ; => '(10)
(filter positive? '(1 -2 3 4 -5)) ; => '(1 3 4)
```

**`foldr`** and **`foldl`** take a *procedure*, an initial value, and one or more lists. They apply the procedure between *n* elements at a time, starting respectively from left and right, using the last iteration result as the first element of next iteration:
```scheme
(foldl cons '() '(1 2 3 4)) ; '(4 3 2 1)
(foldl (lambda (a b result)
           (* result (- a b)))
         1
         '(2 2 2)
         '(4 5 6)) ; (((1 * -4) * -3)) * - 2) => -24
(foldl string-append "" '("bella" "è" "vita" "la")) ; => "lavitaèbella"

(foldl (lambda (v t) (cons (+ 1 v) t)) '() '(1 2 3 4)) ; => '(5 4 3 2)
(foldr (lambda (v t) (cons (+ 1 v) t)) '() '(1 2 3 4)) ; => '(2 3 4 5)
```

>[!help]- Explanation
>`foldl` is tail recursive because it processes the list from left to right, accumulating the result as it goes. At each step, it applies the function to the current element and the accumulated result, and then immediately passes this new result to the next recursive call. Because the result of the recursive call is not further processed (i.e., there are no operations pending after the recursive call), `foldl` is tail recursive. Tail recursion is a desirable property because tail-recursive functions can be optimized by the compiler to use constant stack space, avoiding potential stack overflow errors for large inputs.
> 
> `foldl` is tail recursive while `foldr` isn't, if we try to make `foldr` tail recursive using closures we end up doing a function composition which instead of using memory from the stack, uses memory from the heap.  
> 
> How `foldl` works on the given example:
> ```scheme
> (foldl (lambda (v t) (cons (+ 1 v) t)) '() '(1 2 3 4)) 
> => (foldl (lambda (v t) (cons (+ 1 v) t)) '(2) '(2 3 4)) 
> => (foldl (lambda (v t) (cons (+ 1 v) t)) '(3 2) '(3 4)) 
> => (foldl (lambda (v t) (cons (+ 1 v) t)) '(4 3 2) '(4)) 
> => (foldl (lambda (v t) (cons (+ 1 v) t)) '(5 4 3 2) '()) 
> => '(5 4 3 2)
> ```
> In `foldl` the initial value and the last iteration result are the "last arguments" of the procedure. 
> 
> On the other hand, `foldr` is not tail recursive because it processes the list from right to left, making a recursive call for each element before applying the function. This means that the result of the recursive call is further processed (the function is applied to the current element and the result of the recursive call).
> 
> How `foldr` works on the given example:
> ```scheme
> (foldr (lambda (v t) (cons (+ 1 v) t)) '() '(1 2 3 4))
> 
> => (cons (+ 1 1) (foldr (lambda (v t) (cons (+ 1 v) t)) '() '(2 3 4)))
> => (cons (+ 1 1) (cons (+ 1 2) (foldr (lambda (v t) (cons (+ 1 v) t)) '() '(3 4))))
> => (cons (+ 1 1) (cons (+ 1 2) (cons (+ 1 3) (foldr (lambda (v t) (cons (+ 1 v) t)) '() '(4)))))
> => (cons (+ 1 1) (cons (+ 1 2) (cons (+ 1 3) (cons (+ 1 4) '()))))
> => '(2 3 4 5)
> ```



### Macros

Macros in Racket are a powerful tool that allows you to extend the language's syntax. They operate on syntax objects, which are essentially s-expressions or identifiers bundled with some information about their location in the source code.  

A *pattern-based* macro replaces any code that matches a pattern and generates output syntax based on that pattern, this last step is called expansion. Macros are expanded at ***compile-time***. 
Macros can be defined through **`define-syntax`** and **`syntax-rules`**, the last is a pair (*pattern* *expansion*).
```scheme
(define-syntax id
  (syntax-rules ()
   [(id . pattern) template])) ; general form
   
(define-syntax while
  (syntax-rules ()          ; no other keywords needed
    [(_ condition body ...) ; pattern P
     (let loop ()           ; expansion of P
       (when condition
         (begin
           body ...
           (loop))))]))
		  
(let ((x 0))
  (while (< x 10)
         (display x)
         (set! x (+ x 1)))) ; => 0123456789
```
`_` in the pattern stands for the macro's name `while` , and `body ...` is the code to be executed inside the loop. The ellipsis `...` means that there can be any number of body expressions, in particular `...` is used to represent zero or more repetitions of the preceding form.

The **`define-syntax-rule`** form is itself a macro that expands into `define-syntax` with a `syntax-rules` form that contains only one pattern and one template. The previous case has only one pattern, it can be re-written as:
```scheme
(define-syntax-rule (while condition body ...)
    (let loop ()
      (when condition
        (begin
          body ...
          (loop)))))
```

In some cases syntax can be expanded in more than one way based on the pattern, lets re-write **`let*`** as a recursive macro:
```scheme
(define-syntax my-let*
  (syntax-rules () 
    [(_ ((var val)) istr ...) ; (= only one variable)
     (( lambda (var) istr ...) val)]
    [(_ ((var val) . rest) istr ...) ; more than one
     (( lambda (var)
         (my-let* rest istr ...)) val )]))

(let ((a 1)(b a)(c b))
  (display e)) ; error =>  a: unbound identifier

(my-let* ((a 1)(b a)(c b))
  (display c)) ; => 1
```

>[!help]- Explanation
>The macro `my-let*` has two rules:
> 1. The first rule matches when the input syntax has only one binding pair. It generates a single lambda function that binds the variable `var` to the value `val`, and then applies the lambda function to `val`. The `istr ...` in the pattern represents the body of the `let*` form, which is placed inside the body of the lambda function. 
> ```scheme
> [(_ ((var val)) istr ...) ;  (= only one variable)
>  (( lambda (var) istr ...) val)]
> ```
> 
> 2. The second rule matches when the input syntax has more than one binding pair. It generates a lambda function for the first binding pair, and the body of this lambda function is a recursive call to `my-let*` with the rest of the binding pairs. This effectively creates a sequence of nested lambda functions. Each lambda function is immediately applied to its corresponding value.
> ```scheme
> [(_ ((var val) . rest) istr ...) ; more than one
>  (( lambda (var)
>      (my-let* rest istr ...)) val )]
> ```
> The last part of the code demonstrates the usage of `my-let*`. It creates three local bindings: `a` is bound to `1`, `b` is bound to `a`, and `c` is bound to `b`. The `display` function is then called with `c` as its argument. Since all the bindings are sequential, `c` will ultimately get the value `1`.
> That wouldn't work with `let` because it binds in parallel.

>[!note] 
>`(let ((x 1)) ...)` can be expressed with a lambda form `((lambda (x) ...) 1)`.  

> [!example]  Literal identifiers in `syntax-rules `
> ```scheme
> ; Define a macro that matches patterns with literal identifiers `add` and `sub`
> (define-syntax my-arith
>   (syntax-rules (add sub)
>     [(my-arith add x y)
>      (+ x y)]  ; When `add` is encountered, replace with (+ x y)
>     [(my-arith sub x y)
>      (- x y)]  ; When `sub` is encountered, replace with (- x y)
>     [(my-arith op x y)
>      (error "Unknown operation: " op)])) ; Handle other operations
> ```
> The literals `add` and `sub`, specified in the parentheses after `syntax-rules` are a list of identifiers that should be treated as literals in the pattern matching. These literals are not replaced by the macro but must match ***exactly*** in the macro usage.

Scheme macros are ***hygienic***, this means that symbols used in their definitions are actually replaced with special symbols not used anywhere else in the program, so it is impossible to have name clashes when the macro is expanded.

### Continuations

A continuation is a value that encapsulates a piece of an expression’s evaluation context.  
**`call-with-current-continuation`** or **`call/cc`** captures the current continuation starting outside the current function call and running up to the nearest enclosing prompt.
For example, in `(+ 1 (+ 1 (+ 1 0)))`, at the point where 0 is evaluated, the expression context includes three nested addition expressions. We can grab that context by changing 0 to grab the continuation before returning 0.

```scheme
(define saved-k #f)   ; dummy var
(define (save-it!)
  (call-with-current-continuation
   (lambda (k)        ; k is the captured continuation
     (set! saved-k k) ; we save it in saved-k
     0)))

(+ 1 (+ 1 (+ 1 (save-it!)))) ; => 3
```

>[!help]- Explanation
> 1. `saved-k` is a global variable that will hold the continuation. It is initially set to `#f`
> 2. `save-it!` is a function that uses the `call-with-current-continuation` function (also known as `call/cc`).
> 	- `call/cc` is a control operator that allows a program to capture the current continuation and use it later.
> 	- In this case, `call/cc` is given a lambda function that takes a single argument `k`. This argument will be the captured continuation.
> 	- Inside the lambda function, `set!` is used to update `saved-k` with the continuation `k`.
> 	- The lambda function then returns `0`, which becomes the result of the `call/cc` invocation and thus of the `save-it!` function call.
> 3. The last line is an arithmetic expression that involves calling `save-it!`.
> 	- The expression is evaluated left to right. So, it first evaluates `(+ 1 (+ 1 (+ 1 (save-it!))))`.
> 	- When `(save-it!)` is called, it saves the current continuation (which is equivalent to `(lambda (x) (+ 1 (+ 1 (+ 1 x))))`) into `saved-k` and returns `0`.
> 	- The rest of the expression is then evaluated as `(+ 1 (+ 1 (+ 1 0)))`, which results in `3`.

>[!note] $\lambda$ return value
>In Scheme and Racket, the order of evaluation of function arguments is unspecified. This means that if you have a function call like `(f a b c)`, the interpreter could choose to evaluate `a`, `b`, and `c` in any order.
>
>However, within the body of a function or a lambda expression, expressions are evaluated in order from top to bottom, and the value of the last expression is returned as the function's result. This is known as the "value of a procedure". This is why in the lambda body the last evaluated expression is `0` and therefore it's the return value.


The continuation saved in `save-k` encapsulates the program context `(+ 1 (+ 1 (+ 1 ?)))`, where `?` represents a place to plug in a result value, because that was the expression context when `save-it!` was called. The continuation is encapsulated so that it behaves like the function `(lambda (v) (+ 1 (+ 1 (+ 1 v))))`:
```scheme
(saved-k 0)             ; => 3
(saved-k 10)            ; => 13
(saved-k (saved-k 0))   ; => 3  ; it is not composable!
```
Applying the captured continuation first aborts (to the current prompt) before restoring the saved continuation, hence the inner `(saved-k 0)` is the only continuation restored.  

**`call-with-composable-continuation`** is useful if we want to nest a continuation inside a larger prompt, the last example `(saved-k (saved-k 0)) ; => 6`.  

We could use `call/cc` as an ***escape procedure***:
```scheme
(+ 3
   (call/cc
    (lambda (exit)
      (for-each (lambda (x)
                  (when (negative? x)
                    (exit x)))
                '(54 0 37 -3 245 19))
      10))) ; => 0
```

>[!help]- Explanation
>1. `(+ 3 ...)` is a simple addition operation in Racket. It adds 3 to the result of the inner expression.
> 2. `call/cc` is a function that stands for "call with current continuation". It takes a single-argument function as an argument, and applies it to the current continuation.
> 3. The argument to `call/cc` is a lambda function that takes a single argument `exit`. This argument represents the current continuation at the point where `call/cc` is called.
> 4. Within this lambda function, `for-each` is used to iterate over the list `'(54 0 37 -3 245 19)`. For each element `x` in the list, it checks if `x` is negative.
> 5. When a negative number is found, `(exit x)` is called. This causes the current continuation to be invoked with `x` as an argument. Since the continuation represents the point at which `call/cc` was called, this effectively jumps out of the `for-each` loop and back to the `call/cc` call, with `x` as the result.
> 6. If no negative number is found in the list, the lambda function returns `10`, which is then added to `3`.
> 
> So in this example, the loop encounters `-3`, causing the `(exit x)` continuation to be invoked with `-3` as an argument. This jumps back to the `call/cc` call, so the result of the `call/cc` expression is `-3`. Adding `3` to this gives `0`, which is the final result of the whole expression.  

Or we can use it to continue from a saved state multiple times:
```scheme
(define saved-cont #f) ; place to save k
(define (test-cont)
  (let ((x 0))
    (call/cc
     (lambda (k)             ; k contains the continuation
       (set! saved-cont k))) ; here is saved
    ;; this is the continuation
    (set! x (+ x 1))
    (display x)
    (newline)))


(test-cont)   ; => 1
(saved-cont)  ; => 2
(define other-cont saved-cont)
(test-cont)   ; => 1 (here we reset saved-cont)
(other-cont)  ; => 3 (other is still going ...)
(saved-cont)  ; => 2
```

Hygienic macros may be a problem if we want introduce new ideas, e.g., `For` syntax, like:
```scheme
(For i from 1 to 10
  do
  (displayln i)
  (when (= i 5)
    (break)))
```
The first `i` will be replaced in the expansion, this means that `i` in the macro definition and `i` in the macro usage context are treated as distinct variables unless explicitly handled. 
A simple solution using an *escape procedure*:
```scheme
(define-syntax For
  (syntax-rules (from to break: do)
    ((_ var from min to max break: br-sym do body ...)
     (let* ((min1 min)
            (max1 max)
            (inc (if (< min1 max1) + -)))
       (call/cc (lambda (br-sym)
                  (let loop ((var min1))
                    body ...
                    (unless (= var max1)
                      (loop (inc var 1))))))))))

(For i from 1 to 10 break: get-out
	 do (display i)
	 (when (= i 5)
	   (get-out))) ; => 12345
```

> [!help]- Explanation
> We define a new syntax `For` that creates a loop construct similar to `for` loops in other languages, with a designated break symbol:
> 1. `define-syntax` is used to create a new syntax (or macro) named `For`.
> 2. `syntax-rules` specify the keywords used in the macro. In this case, `from`, `to`, `break:` and `do` are the keywords. The underscore `_`  matches the name of the macro `For`.
> 4. The `var from min to max break: br-sym do body ...` pattern matches a `For` expression with the specified structure. The `var`, `min`, `max`, `br-sym`, and `body ...` are pattern variables that get bound to the corresponding parts of the `For` expression when it's expanded.
> 5. The `let*` form is used to define local variables `min1`, `max1`, and `inc`. The `inc` variable is a function that increments or decrements `var`, depending on whether `min1` is less than `max1`.
> 6. The `call/cc` function is a control operator that captures the current continuation (the rest of the program) as a procedure. This is what allows the `br-sym` symbol to break the loop.
> 7. The `let loop` statement defines a recursive function that represents the loop. If `var` is not equal to `max1`, it calls itself with `var` incremented or decremented by 1.
>    
> In the usage of the `For` macro, `i` is the variable, 1 and 10 are the minimum and maximum values, `get-out` is the symbol for breaking the loop, and `(display i)` and `(when (= i 5) (get-out))` are the body of the loop.
> This means it will display the value of `i` for each iteration of the loop, and break the loop when `i` equals 5, displaying the numbers 1 through 5.

> [!note] Another type of "Escape Sequence"
`call/cc` captures the current continuation (essentially, the current point in the program execution) and binds it to `br-sym`, allowing the loop to be exited prematurely when `br-sym` is called: when you call `get-out` within the loop, you're effectively invoking the continuation that was captured by `call/cc`. This continuation represents the state right after the `call/cc` call but before entering the loop. 
Invoking this continuation with `get-out` "jumps" out of the loop and resumes execution from that point, effectively bypassing any further loop iterations and any code following the invocation of `get-out` within the loop's body.
In this case the "escape sequence" (or break symbol) is in the body of the macro, the *call/cc* is used for its abort effect when it is called, which happens when in the body `get-out` is evaluated. If we had used `call-with-composable-continuation` we would cycle through the entire range.
> 
> ```scheme
> (define-syntax For
>   (syntax-rules (from to break: do)
>     ((_ var from min to max break: br-sym do body ...)
>      (let* ((min1 min)
>             (max1 max)
>             (inc (if (< min1 max1) + -)))
>        (call-with-composable-continuation (lambda (br-sym)
>                   (let loop ((var min1))
>                     body ...
>                     (unless (= var max1)
>                       (loop (inc var 1))))))))))
> 
> (For i from 1 to 10 break: get-out
>      do (display i)
>      (when (= i 5)
>        (get-out))) ; => 12345678910
>  ```

>[!note]
We define `min1` and `max1` with `let*` because they could be any (expensive) expression, so we want to force the evaluation once. Moreover, if `min` or `max` have side-effects (e.g. iterators) each expansion would end in a different value, leading to undesired results.
While `let` itself doesn't force evaluation, the sequential nature of `let*` does cause `min` and `max` to be evaluated in this context.

### Exceptions 

Scheme standards provide already exception handling but, for the sake of learning we are going to create a simple exception system ourselves.  

The first thing we need is a stack for installed handlers:
```scheme
(define *handlers* (list)) ; list == '()

(define (push-handler proc)
  (set! *handlers* (cons proc *handlers*)))

(define (pop-handler)
  (let ((h (car *handlers*)))
    (set! *handlers* (cdr *handlers*)) h))
```

**`throw`**: if there's a handler, we pop it and call it, otherwise we raise an error:
```scheme
(define (throw x)
  (if (pair? *handlers*) ; same as (not (eqv? '() *handlers*))
      ((pop-handler) x)
      (apply error x)))
```

`error` is an already defined procedure in Scheme, it takes a string as a mandatory first argument, which is the error message, and optional additional arguments, which are included in the error report.
`(apply error x)` applies the `error` function to the elements in the list `x`. If `x` is a list of strings, the first string will be used as the error message, and the rest of the strings will be included in the error report.

**`try-catch`** macro:
```scheme
(define-syntax try
  (syntax-rules (catch)
    ((_ exp1 ...
        (catch what hand ...))
     (call/cc (lambda (exit) ; install the handler
                (push-handler (lambda (x)
                                (if (equal? x what)
                                    (exit
                                     (begin
                                       hand ...))
                                    (throw x))))
                (let ((res ;; evaluate the body
                       (begin exp1 ...)))
                  ; ok: discard the handler
                  (pop-handler)
                  res))))))
```

>[!help]- Explanation
The macro `try` takes a set of expressions `exp1 ...`, and an exception handler in the form of `(catch what hand ...)`. The handler consists of a condition `what` and an expression `hand ...` to be executed when the condition is matched.
> 1. `call/cc` is called with a lambda function that takes `exit` as an argument.
> 2. `push-handler` is called with another lambda function that takes `x` as an argument. This function checks if `x` is equal to `what`. If true, it calls `exit` with the result of evaluating `hand ...` expressions. If false, it re-throws the exception with `x`.
> 3. A `let` expression is used to evaluate the body of the `try` block, `exp1 ...`. The result of the evaluation is bound to `res`.


Example:
```scheme
(define (foo x)
  (display x) (newline)
  (throw "hello"))

(try
 (display "Before foo ")
 (newline)
 (foo "hi!")
 (display "After foo") ; unreached code
 (catch "hello" ; this is the handler block
        (display "I caught a throw.") (newline)
        #f))

;; => Before foo 
;;    hi!
;;    I caught a throw.
;;    #f
```

### Closure as objects

A class is a definition of a type (set).  
An object is a value of a type (element of a set).

Defining classes with closures in Scheme is the only way of having local data, which can be accessed by methods:
```scheme
;; class
(define (make-simple-object)
  (let ((my-var 0)) ; attribute
    
    ;; methods:
    (define (my-add x)
      (set! my-var (+ my-var x)) my-var)
    (define (get-my-var) my-var)
    (define (my-display)
      (newline)
      (display "my Var is: ")
      (display my-var)
      (newline))
    (lambda (message . args)
      (apply (case message
               ((my-add) my-add)
               ((my-display) my-display)
               ((get-my-var) get-my-var)
               (else (error "Unknown Method!")))
             args))))
			 
(define a ( make-simple-object ))
(define b ( make-simple-object ))
(a 'my-add 3) ; => 3
(a 'my-add 4) ; => 7
(a 'get-my-var) ; => 7
(b 'get-my-var) ; => 0
(a 'my-display) ; => My Var is: 7
```

> [!note] (Methods) and Variables
Even if some methods do not have arguments, we still need to add parentheses around the method name when defining them. This is because the method name is treated as a function call, and the parentheses are required to indicate that it's a function call with no arguments. If we omit the parentheses, the method name would be treated as a variable reference, which would cause a different behavior.

For example, `(define (my-display) ...)` defines a function named `my-display`, while `(define my-display ...)` would define a variable named `my-display` with the value of the following expression, that means it will evaluate the expression as soon as possible (and bind the result to the variable) instead of making it a callable function.

*Inheritance* is code reuse, which is desirable, how can we implement it? 

***Inheritance by delegation:***
```scheme
(define (make-son)
  (let ((parent (make-simple-object)) ; inheritance
        (name "an object"))
		
    (define (hello) "hi!")
    (define (my-display)
      (display "My name is ")
      (display name)
      (display " and")
      (parent 'my-display))
    (lambda (message . args)
      (case message
        ((hello) (apply hello args ))
        ;; overriding
        ((my-display) (apply my-display args))
        ;; parent needed
        (else (apply parent (cons message args)))))))

(define c (make-son))
(c 'my-add 2)
(c 'my-display); => My name is an object and
               ;    my Var is: 2
(display (c 'hello )) ; => hi!
```


### Proto-oo

*Prototype-based* object orientation:
- there are no classes, new objects are obtained by *cloning* and modifying existing objects
- *hash tables* are the main data structure to implement objects

Keys are symbols, either of an attribute (data) or method (function).
An object is implemented with a hash table:
```scheme
(define new-object make-hash)
(define clone hash-copy)
```

**`make-hash`** takes a list of pairs and creates an hash table.
**`hash-copy`** takes an hash table and returns a mutable hash table with the same mappings.
**`hash-set`** takes an hash table, a key and a value, it maps each key to each value in the hash table.

**`hash-ref`** is a function used to retrieve a value from a hash table given a specific key. If the key is present in the hash table, `hash-ref` returns the associated value. Here's the basic usage:
```scheme
(hash-ref hash-table key [failure-result])
```
- `hash-table` is the hash that you want to look up.
- `key` is the key for which you want to find the corresponding value.
- `failure-result` is an optional argument. If provided and the key is not found in the hash, `hash-ref` will return this value instead of raising an error.

Here's an example:
```scheme
; Create a hash table
(define my-hash (make-hash 'a 1 'b 2 'c 3))

; Retrieve value associated with key 'a
(define value-a (hash-ref my-hash 'a))
(displayln value-a) ; Output: 1

; Attempt to retrieve value with key 'd', which does not exist in the hash
(define value-d (hash-ref my-hash 'd "Key not found"))
(displayln value-d) ; Output: "Key not found"
```

Object methods can be implemented using macros: we can avoid quoting the *keys*.
```scheme
(define-syntax !! ;; setter
  (syntax-rules ()
    ((_ object msg new-val)
     (hash-set! object 'msg new-val))))
     
(define-syntax ?? ;; reader
  (syntax-rules ()
    ((_ object msg)
     (hash-ref object 'msg))))
     
(define-syntax -> ;; send message
  (syntax-rules ()
    ((_ object msg arg ...)
     ((hash-ref object 'msg) object arg ...))))
```

First, we define an object and its methods:
```scheme
(define new-object make-hash)
(define clone hash-copy)

(define Pino (new-object))
(!! Pino name "Pino") ;; slot added
(!! Pino hello
    (lambda (self x) ;; method added
      (display (?? self name ))
      (display ": hi, ")
      (display (?? x name ))
      (display "!")
      (newline)))

(!! Pino set-name
    (lambda (self x)
      (!! self name x)))
(!! Pino set-name-&-age
    (lambda (self n a)
      (!! self name n)
      (!! self age a)))
```

And a clone:
```scheme
(define Pina (clone Pino))
(!! Pina name "Pina")
```

Using the example:
```scheme
(-> Pino hello Pina) ; Pino: hi , Pina!
(-> Pino set-name "Ugo")
(-> Pina set-name-&-age "Lucia" 25)
(-> Pino hello Pina) ; Ugo: hi , Lucia!
```

Inheritance is not typical of prototype object systems, still is implemented as *inheritance by delegation*:
```scheme
(define (deep-clone obj)
  (if (not (hash-ref obj '<<parent>> #f))
      (clone obj)
      (let* ((cl (clone obj))
             (par (?? cl <<parent>>)))
        (!! cl <<parent>> (deep-clone par)))))
(define (son-of parent)
  (let ((o (new-object )))
    (!! o <<parent>> (deep-clone parent ))
    o))
```

Deep cloning clones also the ancestors of the object:
- check if the object has a slot called parent
`(hash-ref obj '<<parent>> #f)` in this case we provide a standard default value `#f` in the key is not present in the hash table

Basic dispatching:
```scheme
(define (dispatch object msg)
  (if (eq? object 'unknown)
      (error "Unknown message" msg)
      (let ((slot (hash-ref
                    object msg 'unknown)))
        (if (eq? slot 'unknown)
            (dispatch (hash-ref object
                                '<<parent>>
                                'unknown) msg)
            slot))))
```

>[!help]- Explanation
>The purpose of the `dispatch` procedure in the context of proto-object-oriented programming in Scheme is to dynamically determine which piece of code (usually a function or procedure) should handle a given message sent to an object. This mechanism is essential for achieving polymorphism in a programming environment that does not have built-in support for object-oriented paradigms.
> 
> The `dispatch` procedure operates as follows:
> 
> - It receives an "object," which is typically a hash table containing key-value pairs that represent the object's slots (properties and methods), and a "msg," which is a symbol representing the message to be dispatched to the object.
> - It checks whether the object is known. If the object is `'unknown`, it raises an error because it can't dispatch a message to an unknown object.
> - If the object is known, it attempts to find the corresponding slot for the `msg` in the object. If the slot is found, it returns the value associated with that slot (which could be a procedure representing a method).
> - If the slot for the message is not found, it means the object does not have a direct handler for the message. The `dispatch` procedure then attempts to find a handler in the object's prototype (parent). It does this by recursively calling itself with the object's parent (retrieved using `hash-ref` on the key `'<< parent >>`) and the same `msg`.
> - This process continues until either a handler is found, or there are no more parents to search through, in which case an error is raised for the unhandled message.

This dispatching mechanism is a fundamental part of implementing a prototype-based object system in Scheme. It allows objects to inherit behavior from their prototypes (parents) and supports polymorphism, as the same message can be handled differently by different objects depending on their specific implementations or the implementations of their prototypes.

We now have to modify `??` and `->` for dispatching:
```scheme
(define-syntax ?? ;; reader
  (syntax-rules ()
    ((_ object msg)
     (dispatch object 'msg))))

(define-syntax -> ;; send message
  (syntax-rules ()
    ((_ object msg arg ...)
     (( dispatch object 'msg) object arg ...))))
```

Example:
```scheme
(define Glenn (son-of Pino))
(!! Glenn name "Glenn")
(!! Glenn age 50)
(!! Glenn get-older
    (lambda (self)
      (!! self age (+ 1 (?? self age )))))
(-> Glenn hello Pina) ; Glenn: hi , Lucia!
(-> Glenn ciao) ; error: Unknown message
(-> Glenn get-older) ; Glenn is now 51
```