## Haskell

We will introduce some basic concepts of Haskell using the Scheme metalanguage.  
Haskell is a ***pure*** functional language, as in mathematics, its functions do not have *side-effects*.  
Scheme is mainly functional but some expressions have side-effects, e.g., `vector-set!` and other procedures marked with  `!`.

### Evaluation strategy: call-by-need

In absence of side-effects (purely functional computations), from the point of view of the *result* the order in which functions are applied (almost) does not matter.  

However, it matters in terms of performances, consider this function:
```scheme
(define (sum-square x y)
  (+ (* x x)
     (* y y)))
```

In Scheme the function is evaluated in a **call-by-value** strategy:
```scheme
(sum-square (+ 1 2) (+ 2 3))
;; applying the first +
= (sum-square 3 (+ 2 3))
;; applying +
= (sum-square 3 5)
;; applying sum-square
= (+ (* 3 3)(* 5 5))
...
= 34
```
This means that *redexs* (reducible expressions) are evaluated in a leftmost-innermost order.

Another strategy is **call-by-name**:
```scheme
(sum-square (+ 1 2) (+ 2 3))
;; applying sum-square
= (+ (* (+ 1 2)(+ 1 2))(* (+ 2 3)(+ 2 3)))
...
= 34
```
In this case *redexs* are evaluated in an outermost fashion, that means function are always applied before their arguments.

If there is an evaluation for an expression that *terminates*, **call-by-name**
terminates, and produces the same result.

>[!note] CBV vs CBN
>- **Call-by-Value (CBV):** In CBV, function arguments are evaluated before the function is called. The result of the evaluation is then passed to the function. Most programming languages, including Racket, use this strategy by default.
> - **Call-by-Name (CBN):** In CBN, function arguments are not evaluated before the function is called. Instead, the arguments are substituted directly into the function body, and their evaluation is deferred until they are actually used within the function. Racket supports CBN evaluation through the `delay` and `force` constructs.
> 

Differences arise when an evaluation *does not terminate*, e.g., infinite loop, in this case only with a **call-by-name** strategy the computation can terminate.
```scheme
(define (infinity)
  (+ 1 (infinity)))
(define (fst x y) x)

(fst 3 (infinity)) ; => 3 with CBN
```

>[!help]- Explanation
>`infinity` represent an infinite computation.
> Let's analyze the code's behavior based on these two evaluation strategies:
> 1. **Call-by-Value (CBV):** In a CBV strategy, `(infinity)` would be evaluated first. Since it's an infinite computation, the evaluation would never terminate. Therefore, the entire expression `(fst 3 (infinity))` would also never terminate because it depends on the infinite evaluation of `(infinity)`.
> 2. **Call-by-Name (CBN):** In a CBN strategy, the arguments are not evaluated before the function call. Therefore, `(infinity)` is not evaluated immediately. If the function `fst` does not use its second argument (i.e., it doesn't need the value of `(infinity)`), then the expression `(fst 3 (infinity))` could terminate. In this case, the infinite computation in `(infinity)` is deferred until it is actually needed, and if it's never needed, it won't be evaluated.

Haskell strategy is ***call-by-need***, which is a *memoized*[^memoized] version of **call-by-name** where, if the function argument is evaluated, that value is stored for subsequent uses.

In Scheme the default is call-by-value, but we can achieve other strategies too:
- call-by-name (outermost order): the only way to delay the execution is to use macros which are expanded at compile time.
- call-by-need: can be enforced using **thunks**[^thunks] to prevent evaluation, and then force it with an explicit call, a possible idea to overcome the non termination of `(fst 3 (infinity))`. 

There is already an implementation in Racket based on *delay* and *force*, but we’ll see how to implement them with macros and thunks.

**`delay`** is used to return a *promise* to execute a computation (implements call-by-name), moreover, it caches the result (memoization) of the computation on its first evaluation and returns that value on subsequent calls (implements call-by-need).
```scheme
(struct promise
  (proc   ; thunk or value
   value? ; already evaluated?
   ) #:mutable)

(define-syntax delay
  (syntax-rules ()
    ((_ (expr ...))
     (promise (lambda ()
                (expr ...)) ; a thunk
              #f)))) ; still to be evaluated
```

**`force`** is used to force the evaluation of a promise:
```scheme
(define (force prom)
  (cond
    ; is it already a value (not a promise)?
    ((not (promise? prom)) prom)
    ; is it an evaluated promise?
    ((promise-value? prom) (promise-proc prom))
    (else
     (set-promise-proc! prom
                        ((promise-proc prom)))
     (set-promise-value?! prom #t)
     (promise-proc prom))))
```

>[!example]
> ```scheme
> (define x (delay (+ 2 5))) ; a promise
> (force x) ;; => 7
> 
> (define lazy-infinity (delay (infinity)))
> (force (fst 3 lazy-infinity))         ; => 3
> (fst 3 lazy-infinity)                 ; => 3
> (force (delay (fst 3 lazy-infinity))) ; => 3
> ```


### Currying

In Haskell functions have only one argument, functions with more arguments are *curried*.  
Consider the function with signature *sum-square*: $\mathbb{C^2} → \mathbb{C}$, where $\mathbb{C}$ is the set of complex numbers:
```scheme
(define (sum-square x y)
  (+ (* x x)
     (* y y)))
```

The curried version has signature *sum-square* : $\mathbb{C} → (\mathbb{C} → \mathbb{C})$:
```scheme
(define (sum-square x)
  (lambda (y)
    (+ (* x x)
       (* y y))))
 
;; shorter version:
(define ((sum-square x) y)
  (+ (* x x)
     (* y y)))
```
The $→$ operator is right associative, the signature can be re-written as *sum-square* : $\mathbb{C} → \mathbb{C} → \mathbb{C}$. 
In the curried version the partial application is much easier: we don't provide all the parameters but just the first, the rest of the parameters are left as lambda functions.

In Haskell every function is automatically curried.

### Types

Haskell has *static typing*, everything must be known at compile time. There is *type inference*, so usually we do not need to explicitly declare variables. Types start with a capital letter.

*"has type"* is written `::`
```haskell
5   :: Integer
'a' :: Char
inc :: Integer -> Integer
[1, 2, 3] :: [Integer]  -- equivalent to 1:(2:(3:[]))
(’b’, 4)  :: (Char, Integer)
"Hello World" :: [Char]
"But also" :: String
```

***Type synonyms*** are defined with keyword *type*:
```haskell
type String = [Char]
type Assoc a b = [(a,b)]
```

***User defined types*** are based on *data declarations*:
```haskell
-- a "sum" type (union in C)
data Bool = False | True
```
`Bool` is the (nullary) type constructor, while `False` and `True` are data constructors (nullary as well). The `Bool` type is already defined in Haskell.

Data and type constructor live in separate name-spaces, so it is possible to use the same name for both:
```haskell
-- a "product" type (struct in C)
data Pnt a = Pnt a a
```

>[!help]- Explanation
>In Haskell, a type constructor is used in the declaration of algebraic data types (ADTs). It is the name of the type itself, not a function to create that type. On the other hand, a data constructor is used to create a new instance of that type.
> 
> In the given example `data Pnt a = Pnt a a`, `Pnt` is used as both a type constructor and a data constructor:
> - As a type constructor, `Pnt` declares a new type that takes a single type parameter `a`. This could be any type, like `Int`, `String`, etc. We use the type constructor when we want to declare a variable or function of that type. For example, `var :: Pnt Int` declares a variable `var` of type `Pnt Int`.
> - As a data constructor, `Pnt` is a function that takes two arguments of type `a` and returns a value of type `Pnt a`. We use the data constructor to create a new value of that type. For example, `var = Pnt 3 4` creates a new value of type `Pnt Int`.
>   
> The term "product type" in the comment refers to the fact that the `Pnt` type combines two values of type `a` into a single value. This is similar to how a struct in C can combine multiple values into a single value.

>[!note] Algebraic Data Types
>Algebraic Data Types (in short ADTs - however we will use this acronym to refer to Abstract Data Types) in Haskell are a way to create your own custom data types. They are called "algebraic" because they can be seen as the result of two operations, "sum" and "product".
> 
> A "sum" type, also known as a variant or union type, is a type that can have several different forms or cases. Each case is usually associated with a different data constructor. For example, in the `Maybe` type:
> ```haskell
> data Maybe a = Nothing | Just a
> ```
> 
> `Maybe` is a "sum" type with two cases: `Nothing` and `Just a`. This is like a logical OR operation: a value of type `Maybe a` is either `Nothing` or `Just a` .
> 
> A "product" type, also known as a record or struct, is a type that combines several values together. This is like a logical AND operation: a value of a "product" type includes several other values. For example, in the `Pnt` type:
> ```haskell
> data Pnt a = Pnt a a
> ```
> `Pnt` is a "product" type that combines two values of type `a`.
> 
> In addition to "sum" and "product" types, ADTs in Haskell can also include recursive types, where a type is defined in terms of itself. For example, the list type in Haskell is defined as:
> ```haskell
> data [a] = [] | a : [a]
> ```
> This is a recursive type, as the definition of the list type refers to the list type itself.

***Parametric polymorphism***: lower case letters are *type variables*, `[a]` stands for *"list of elements of type a, for any (type) a"*.  
It allows a function or a data type to be written generically so that it can handle values identically without depending on their type.

Product types are like struct in C or in Scheme, the access is positional, we may define accessors as:
```haskell
pointx Point x _ = x
pointy Point _ y = y

-- OR with C like syntax
data Point = Point {pointx, pointy :: Float}
```
This declaration automatically define two field names `pointx` and `pointy` and their corresponding selector functions.  

>[!note]
>`_` means *don't care*, in regex terms is equivalent to `*` .
>
For example:
> ```haskell
> case x of
>   0 -> "zero"
>   _ -> "non-zero"
> ```
In this case, `_` matches any value that isn't zero.

***Recursive types*** are allowed, an example of binary tree:
```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a) 
Branch :: Tree a -> Tree a -> Tree a  -- data constructor Branch type
aTree = Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))
```

At the prompt we can use `:t` for getting the type:
```haskell
ghci> :t (aTree)
(aTree) :: Tree Char
```

List are also recursive, Haskell has special syntax for them:
```haskell
data [a] = [] | a : [a]
``` 
***Note:*** `[]` is a data and type constructor, while `:` is an infix data constructor.

>[!note] `:` cons operator and lists
>- `[]` is both a type constructor and a data constructor. As a type constructor, it represents the type of lists. For example, `[Int]` is the type of lists of integers, and `[Char]` is the type of lists of characters (or `String`). As a data constructor, it represents an empty list. For example, in the expression `[] :: [Int]`, `[]` is an empty list of integers.
> - `:` is an infix data constructor, also known as the cons operator. It constructs a new list by prepending an element to an existing list. For example, in the expression `1:2:3:[]`, `:` is used to construct a list of integers [1, 2, 3]. The `:` operator is right associative, so `1:2:3:[]` is equivalent to `1:(2:(3:[]))`

### Functions

Functions are declared through a sequence of *equations*:
```haskell
length :: [a] -> Integer      -- domain and codomain
length [] = 0                 -- case of empty list covered first
length (x:xs) = 1 + length xs -- general case
```
A list of integers can be seen as a cons node `(x:xs)` where `x` is the first value and `xs` is the rest of the list.

>[!note] Type Inference
>Type inference in Haskell is a feature of the type system that allows the compiler to deduce the concrete types of variables and functions wherever it is obvious. This means that you don't have to explicitly specify the types of variables and functions in your code, and the compiler will figure out the types for you [1](https://wiki.haskell.org/Type_inference).
> ```haskell
> length [] = 0                 -- case of empty list covered first
> length (x:xs) = 1 + length xs -- general case
> 
> ghci> :t length
> length :: Foldable t => t a -> Int
> ```

This is also a case of ***pattern matching***: arguments are matched with the right part of the equations and if a match succeeds, the function body is called.

*Pattern matching* proceeds top-down, left-to-right, patterns may have *boolean guards*:
```haskell
sign x | x > 0  = 1
       | x == 0 = 0
       | x < 0  = -1
```

An example function on trees:
```haskell
fringe :: Tree a -> [a]      -- domain and codomain
fringe (Leaf x) = [x]        -- special case first
fringe (Branch left right) = fringe left ++ fringe right
```
 `++` is a list concatenation.

Haskell has **`map`**, and it can be defined as:
```haskell
map f [] = []                 -- special case first 
map f (x:xs) = f x : map f xs -- general case

map (1 +) [1,2,3] -- => [2,3,4]
```

 `.` is used for composing functions 
```haskell
let dd = (*2) . (1+)
dd 6 -- => 14

let dd = (2*) . (+1)
dd 6 -- => 14
```

`$` syntax for avoiding parentheses, e.g. `(10*) (5+3) = (10*) $ 5+3`

>[!note] `$` syntax
>The `$` syntax is used as an alternative to parentheses in function application. 
>The `$` operator has the lowest precedence, so it is often used to avoid the need for explicit parentheses. It works by simply applying the function on its left to the expression on its right.
>For example, instead of writing `(10*) (5+3)`, you can write `(10*) $ 5+3`. The `$` operator allows you to avoid the need for parentheses around the `(5+3)` expression.

***Infinite computations*** can be defined:
```haskell
ones = 1 : ones
numsFrom n = n : numsFrom (n+1)
squares = map (^2) (numsFrom 0)
```

We cannot evaluate them but there is **`take`** to get *finite slices* from them:
```haskell
take 5 squares  -- => [0,1,4,9,16]
```

There is a convenient syntax for *infinite lists*:
```haskell
ones = [1,1..]
numsFrom n = [n..]
```

>[!help] Explanation
>The syntax `[1,1..]` creates an infinite list where all elements are `1`. The `..` operator generates a list that starts from the first element and repeats the difference between the first two elements. Since the difference between `1` and `1` is `0`, all elements in the list are `1`.
> The function `numsFrom n` uses the syntax `[n..]` to generate an infinite list starting from `n`. The difference between the first two elements is not specified, so Haskell assumes a difference of `1` by default.
> 

**`zip`** is a useful function:
```haskell
:t zip             -- => zip :: [a] -> [b] -> [(a, b)]
zip [1,2,3] "ciao" -- => [(1,'c'),(2,'i'),(3,'a')]
```

List comprehension uses set annotation theory (like in Python):
```haskell
[(x,y) | x <- [1,2], y <- "ciao"]
-- => [(1,'c'),(1,'i'),(1,'a'),(1,'o'),(2,'c'),(2,'i'),(2,'a'),(2,'o')]
```
In this case we created a list of pairs where the first element `x` is taken from the first list and `y` from the second.  

A list of all the fibonacci numbers:
```haskell
fib = 1 : 1 :
	[a+b | (a,b) <- zip fib (tail fib)] 
	-- recursive zip [1,1] , [1, __ ]

zip [1,1,2,3] [1,2,3]  -- => [(1,1),(1,2),(2,3)]
```
Similar to Scheme `tail` is *cdr*, while `head` is *car*.

***bottom*** (often represented as ⊥) is a term used to represent computations that do not result in a normal value. 
It is defined as `bot = bot`, an example of a non-terminating computation, the expression is trying to define `bot` in terms of itself, leading to an infinite loop. 
`bottom` can manifest in several ways, such as:
- Non-terminating computations (infinite loops)
- Exceptions
- Partial functions that are not defined for certain inputs
All errors have value `bot`, a value shared by all types.
`error :: String -> a` is used to generate an error and it can be of any type (polymorphic only in the output), the reason is that it returns **`bot`**, in practice an exception is raised.
```haskell
take 0 bot -- => []
take bot 0 -- => infinite computation
```

>[!help] Explanation
>In the first example, `take 0 bot` returns `[]` because `take 0` is defined as returning an empty list, regardless of the second argument. The computation doesn't need to evaluate `bot`, so it doesn't enter an infinite loop or raise an error. This demonstrates Haskell's lazy evaluation.
> 
> In the second example, `take bot 0` results in an infinite computation. This is because the first argument to `take` needs to be evaluated to determine how many elements to take from the list. When Haskell tries to evaluate `bot`, it enters an infinite loop because `bot` is defined as `bot = bot`
> 

### Basics

**`case`** syntax for an alternative definition of `take`:
```haskell
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

-- alternative
take m ys = case (m,ys) of 
              (0,_)    -> []
              (_,[])   -> []
              (n,x:xs) -> x : take (n-1) xs
```

**`if`** is available, its syntax is `if <c> then <t> else <e>`, we could define it as a function:
```haskell
if :: Bool -> a -> a -> a
if True  x _ = x
if False _ y = y
```

**`let`** is like Scheme `letrec` , that means evaluation are top-to-bottom, left-to-right and may be recursive:
```haskell
-- python like layout
let x = 3
    y = 12
in x+y -- => 15

-- C like layout
let {x = 3 ; y = 12} in x+y
```

**`where`** can be convenient to scope binding over equations (local binding):
```haskell
powset set = powset' set [[]] where
  powset' [] out = out
  powset' (e:set) out = powset' set (out ++ [e:x | x <- out])
```

>[!help] Explanation
>The function `powset` generates the power set of a given set. The power set of a set is the set of all subsets of the set, including the empty set and the set itself.
>
>The `powset'` function is defined within the `where` clause, making it local to `powset`. It uses recursion and list comprehension to generate the power set:
> - If the input list is empty (`powset' [] out`), it returns the `out` list, which accumulates the subsets.
> - If the input list is not empty (`powset' (e:set) out`), it calls itself recursively with the tail of the list (`set`) and an updated `out` list. The updated `out` list is the concatenation of the current `out` list and a new list generated by prepending the current element `e` to each subset in the `out` list.

### Strictness in Haskell

**`foldl`** is efficient in Scheme, its definition is naturally *tail-recursive*, in Haskell this is not as efficient because of call-by-need:
```haskell
foldl (+) 0 [1,2,3]
foldl (+) (0 + 1) [2,3]
foldl (+) ((0 + 1) + 2) [ 3 ]
foldl (+) (((0 + 1) + 2) + 3) []
(((0 + 1) + 2) + 3) = 6
```

Haskell is a non-strict (or *lazy*) by default but there are various ways to enforce *strictness* in Haskell, e.g., on data we can use *bang patterns*:
```haskell
data Complex = Complex !Float !Float
```
A datum marked with `!` is considered strict.

**`seq`** is the canonical operator to force evaluation `seq :: a -> t -> t`.  
`seq x y` returns `y` only if the evaluation of x terminates, strict version of **`fodl`**:
```haskell
foldl' f z []     = z
foldl' f z [x:xs] = let z' = f z x
                    in seq z' (foldl' f z' xs)
```
Strict version of standard functions are usually primed.

There is a convenient *strict* variant of `$` called `$!`, its definition:
```haskell
($!) :: (a -> b) -> a -> b
f $! x = seq x (f x)
```

>[!help] Explanation
>The function `$!` takes a function `f` of type `(a -> b)` and an argument `x` of type `a`. It uses the `seq` function to force the evaluation of `x` before applying the function `f` to `x` and returning a result of type `b`.
>
> The `seq` function in Haskell is used to force the evaluation of its first argument, and then return the second argument. In the definition of `$!`, `seq` is used to force the evaluation of `x` before the function `f` is applied to `x`


### Modules 

Haskell has a simple **module** system, with *import*, *export* and namespaces
```haskell
module CartProd where --- export everything
infixr 9 -*-
-- r: right associative
-- 9: precedence goes from 0 to 9, the strongest
x -*- y = [(i,j) | i <- x, j <- y]
```

In Haskell, a file can only define a single module, for example the module `Tree`has to be defined in another file:
```haskell
module Tree ( Tree(Leaf, Branch), fringe ) where
data Tree a = Leaf a | Branch (Tree a) (Tree a)
fringe :: Tree a -> [a] ...
```

>[!note] Tree Module Definition
> The `Tree` module is defined with the keyword `module`, followed by the module's name (`Tree`), and a list of exported entities enclosed in parentheses. In this case, `Tree (Leaf, Branch)` and `fringe` are the exported entities.
> The `data` keyword is used to define a new algebraic data type `Tree` with two constructors: `Leaf` and `Branch`. The `Tree` type is parameterized over a type `a`, meaning it can hold values of any type. `Leaf a` represents a leaf node with a value of type `a`, and `Branch (Tree a) (Tree a)` represents a branch node with two child `Tree` nodes.
> The `fringe` function is declared with a type signature, but its implementation is not shown in the snippet.

The ***main module*** in Haskell is the module that serves as the entry point of a Haskell program. By convention, this module is called `Main` and it must export the value `main`. The `main` function is of type `IO ()` and when the program is executed, the computation `main` is performed, and its result is discarded:
```haskell
module Main (main) where
import Tree ( Tree(Leaf ,Branch) ) -- import Tree
main = print (Branch (Leaf 'a') (Leaf 'b'))
```

Modules provide the only way to build Abstract Data Types (ADT), the characteristic feature of an ADT is that the representation type is hidden: all operations on the ADT are done at an abstract level which does not depend on the representation.

A suitable ADT for binary trees might include the following operations:
```haskell
data Tree a  -- just the type name
leaf         :: a -> Tree a
branch       :: Tree a -> Tree a -> Tree a
cell         :: Tree a -> a
left, right  :: Tree a -> Tree a
isLeaf       :: Tree a -> Bool
```

ADT implementation:
```haskell
module TreeADT (Tree, leaf, branch, cell,
                left, right, isLeaf) where
data Tree a          = Leaf a | Branch (Tree a) (Tree a)
leaf                 = Leaf
branch               = Branch
cell (Leaf a)        = a
left (Branch l r)    = l
right (Branch l r)   = r
isLeaf (Leaf _)      = True
isLeaf _             = False
```
Having an ADT without constructor is an advantage if at a later time we change the representation type without affecting users of the type.

>[!note] Hiding Data Constructors
>`Tree` is the type constructor and `Leaf` and `Branch` are data constructors. When you define an ADT without exporting its data constructors (like `Leaf` and `Branch`), you're hiding the details of how the data type is implemented. This means that code outside the module can only use the functions you've specifically provided to manipulate values of the ADT, and cannot directly use the data constructors.

The `Prelude` module is a standard library module that is automatically imported into every Haskell program by default. It contains a collection of commonly used functions, types, and classes that are essential for writing Haskell programs, in fact all the operations that we didn't define ourselves are part of the `Prelude` module.

### Type classes and overloading

Type classes are the mechanism provided by Haskell for *ad-hoc polymorphism*[^ad-hoc-polymorphism] (a.k.a. overloading), e.g.:
```haskell
:t (+)  -- => (+) :: Num a => a -> a -> a
```
**`Num`** is a type class (to consider as a set of types), `Float`, `Integer`, `Int`[^Int], `Rational` belongs to the class and therefore we can apply `+` to these types.

A class is a "container" of overloaded operations, we can declare a type to be an *instance* of a ***type class***, meaning that it implements its operations. 
In other words, a type class defines a set of functions that can have different implementations depending on the type of data they are used with.

The `Eq` typeclass provides the `==` (equality) and `/=` (inequality) operations. So, for any type that is a member of the `Eq` type class, you can use these operations.
Type class **`Eq`** definition:
```haskell
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)

(==) :: Eq a => a -> a -> Bool
```
In this definition, `a` is a type variable and `Eq a` is a typeclass constraint. The constraint `Eq a` means that `a` must be an instance of `Eq` typeclass.
The `==` and `/=` operations are defined in terms of each other. This means if you define one of them for a type, Haskell can automatically figure out the other one. This is why you can create an instance of `Eq` and only need to implement the `==` operator.
An implementation of `==` is called a *method*.

>[!note] Haskell vs Java
> 
> | Haskell | Java      |
> | ------- |---------- |
> | Class   | Interface |
> | Type    | Class     |
> | Value   | Object    |
> | Method  | Method    |
> 
> A method in Haskell is the implementation of a specific function defined in a (type) class for a specific data type.
> 
> In Java, an *Object* is an instance of a *Class*.
> In Haskell, a *Type* is an instance of a *Class*.

We can define instances like this:
```haskell
instance (Eq a) => Eq (Tree a) where
-- type a must support equality as well
  Leaf a == Leaf b = a == b
  (Branch l1 r1) == (Branch l2 r2) = (l1==l2) && (r1==r2)
  _ == _ = False
```
We declare that `Tree a` is an instance of the `Eq` type class, but only when `a` is also an instance of `Eq` with the `(Eq a) =>` syntax. This is because the implementation of `==` for `Tree a` relies on `==` for `a`. 

We can also extend `Eq` with comparison operations:
```haskell
class (Eq a) => Ord a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a
```
**`Ord`** is a ***subclass*** of `Eq`.
It is possible to have **multiple inheritance**: `class (X a, Y a) => Z a`.  

**`Show`** is another class used for *showing*, i.e. creating a string representation of the value. To have an instance of `Show` we have to implement the method **`show`**:
```haskell
instance Show (a -> b) where
  show f = "<< a function >>"
  
instance Show a => Show (Tree a) where
  show (Leaf a) = show a
  show (Branch x y) = "<" ++ show x ++ " | " ++ show y ++ ">"
```

>[!note] Functions are types
> 1. `instance Show (a -> b) where`: This line declares that function types `(a -> b)` are an instance of the `Show` type class. This means that function values can be converted to strings.
> 2. `show f = "<< a function >>"`: This line defines how to convert a function to a string. No matter what the function `f` does, it will be represented as the string `"<< a function >>"` when converted using `show`.

**`deriving`** can be used to automatically define instances of some classes:
```haskell
-- custom operator right-associative with precedence level of 5
infixr 5 :^: 
data Tr a = Lf a | Tr a :^: Tr a deriving (Show, Eq)

ghci> let x = Lf 3 :^: Lf 5 :^: Lf 2
ghci> x
Lf 3 :^: (Lf 5 :^: Lf 2)
```

Example with class `Ord`:
```haskell
data RPS = Rock | Paper | Scissors deriving (Show, Eq)

instance Ord RPS where
  x <= y | x == y      = True
  Rock     <= Paper    = True 
  Paper    <= Scissors = True
  Scissors <= Rock     = True
  _        <= _        = False
```
In Haskell `|` pipes are guards, `x <= y | x == y` means that if we match any two instances of `RPS` where the boolean guard `x == y` is true then `<=` is true, in other words if `x` and `y` are the same move, then `x` is considered to be less than or equal to `y`. We only need to define `<=` to have an instance of `Ord`.

A simple reimplementation of rational numbers with class `Num`: 
```haskell
data Rat = Rat !Integer !Integer deriving (Eq) -- Rational \frac{x}{y}

simplify (Rat x y) = let g = gcd x y in Rat (x 'div' g) (y 'div' g)

makeRat x y = simplify (Rat x y)

instance Num Rat
	(Rat x y) + (Rat x' y') = makeRat (x * y' + x' * y) (y * y')
	(Rat x y) - (Rat x' y') = makeRat (x * y' - x' * y) (y * y')
	(Rat x y) * (Rat x' y') = makeRat (x * x') (y * y')
	abs (Rat x y)           = makeRat (abs x) (abs y)
	signum (Rat x y)        = makeRat (signum x * signum y) 1
	fromInteger x           = makeRat x 1

instance Ord Rat where
	(Rat x y) <= (Rat x’ y’) = x*y’ <= x’*y

instance Show Rat where
	show (Rat x y) = show x ++ "/" ++ show y
```

>[!help] Explanation
> The `/` operator performs floating-point division, and the `div` function performs integer division.
> The backticks (\`\`) are used to make any function that takes two arguments behave like an operator. This means you can use the function between its two arguments, which can often make your code more readable:
> ```haskell
> div x y -- same as:
> x `div` y
> ```
> The `gcd` is *greatest common denominator* function and it is also part of the `Prelude` module; `let g = gcd x y` calculates the greatest common divisor (gcd) of `x` and `y`.
> The `signum` function is a method in the `Num` type class that takes a number and returns -1 if the number is negative, 0 if the number is zero, and 1 if the number is positive.
> 
> A [demo](https://replit.com/@glazzarelli/Haskell-Code-Snippet#Main.hs) of this snippet made in Replit.


### Input/Output is dysfunctional

I/O is not referentially transparent[^referentially-transparent], two different calls of `getChar` could return different characters.
In general, IO computation is based on state change (e.g. of a file), hence if we perform a sequence of operations, they must be performed in order (and this is not easy with call-by-need).
**`getChar`** is an *IO action*:
```haskell
getChar :: IO Char
putChar :: Char -> IO ()
```
**`IO`** is an instance of the monad class, and in Haskell it is considered as an indelible stain of impurity.

**`main`** is the default entry point of the program (like in C), special syntax for working with IO: `do`, `<-`:
```haskell
main = do {
  putStr "Please, tell me something>";
  thing <- getLine;
  putStrLn $ "You told me \"" ++ thing ++ "\".";
}

main    :: IO ()
putStr  :: String -> IO ()
getLine :: IO String
```
`<-` is used instead of `=` to obtain a value from an IO action.
We will see its real semantics later, it is used to define an IO action as an ordered sequence of IO actions.

In Haskell, exceptions can be raised in pure code but can only be caught within the IO monad. The `handle` function provided serves this purpose. It has the type:
```haskell
Exception e => (e -> IO a) -> IO a -> IO a
```
This means it takes two arguments:
- A function that takes an exception and returns an IO action.
- An IO action where exceptions may occur.

The first argument `(e -> IO a)` is the `handler` which is a function that gets triggered when an exception occurs.
```haskell
import System.IO
import System.Environment

readfile = do {
    args <- getArgs; -- command line arguments
    handle <- openFile (head args) ReadMode;
    contents <- hGetContents handle; -- note: lazy
    putStr contents;
    hClose handle;
    }
    
main = handle handler readfile
    where handler e
        | isDoesNotExistError e =
            putStrLn "This file does not exist."
        | otherwise =
            putStrLn "Something is wrong."    
```

>[!help]- Explanation
>This is a simple script for reading and printing the contents of a file specified via command line argument:
>- `import System.IO` and `import System.Environment`: These lines import the necessary modules for handling file I/O and accessing system environment information, respectively.
> - `args <- getArgs`: This line retrieves the command line arguments provided when running the script. `getArgs` is a function from the `System.Environment` module that returns a list of command line arguments.
> - `handle <- openFile (head args) ReadMode`: This line opens the file whose name is the first command line argument (`head args`) in read mode. The file's handle, which is a reference to the open file, is bound to the variable `handle`.
> - `contents <- hGetContents handle`: This line reads the entire contents of the file into the string `contents`. The `hGetContents` function is lazy, which means it doesn't actually read the file until its contents are needed.
> - `putStr contents`: This line prints the contents of the file to the console.
> - `hClose handle`: This line closes the file handle, freeing up system resources.
> - `main = handle handler readfile`: This line defines the `main` function, which is the entry point of the script. It runs the `readfile` function and passes any exceptions it throws to the `handler` function.
> - `where handler e...`: This part defines the `handler` function, which takes an exception `e` and prints an error message based on its type. If the exception is a "does not exist" error, it prints "This file does not exist." For any other type of exception, it prints "Something is wrong.".


### Other classical data structures

Traditional versions of data structure are imperative! If really needed, there are libraries with imperative implementations living in the IO monad.

In Haskell, the idiomatic approach to working with data structures is to use immutable arrays (`Data.Array`) and maps (`Data.Map`) instead of imperative structures. These are both part of the standard libraries that come with Haskell.

#### Data.Map Data Structure

The `Data.Map` module provides an efficient implementation of ordered maps from keys to values (dictionaries) using balanced binary trees. Its `find` operation is $O(\log ⁡n)$, and `update` operation is also $O(\log n)$.

Here is an example of how you might use a map in Haskell:
```haskell
import Data.Map

exmap = let m = fromList [("nose", 11), ("emerald", 27)]
            n = insert "rug" 98 m
            o = insert "nose" 9 n
    in (m ! "emerald", n ! "rug", o ! "nose")
    
ghci> exmap
(27,98,9)
```
In this example, `fromList` is used to create a map from a list of pairs. The `insert` function is used to add new key-value pairs to the map. The `!` operator is used to read from the map. Note that each time a value is inserted into the map, a new map is created instead of modifying the existing one, as the `Data.Map` in Haskell is immutable.

#### Data.Array Data Structure

The `Data.Array` module provides immutable arrays, which are contiguous, zero-based arrays. The `find` operation is $O(1)$, and the `update` operation is $O(n)$.

Here is an example of how you might use an array in Haskell:
```haskell
import Data.Array

exarr =
  let m = listArray (1, 3) ["alpha", "beta", "gamma"]
      n = m // [(2, "Beta")]
      o = n // [(1, "Alpha"), (3, "Gamma")]
   in (m ! 1, n ! 2, o ! 1)
   
ghci> exarr
("alpha","Beta","Alpha")
```
In this example, `listArray` is used to create an array from a list, with the range of indexing provided as the first argument. The `//` operator is used to update the array, and the `!` operator is used to read from the array. Note that each time a value is updated in the array, a new array is created instead of modifying the existing one, as the `Data.Array` in Haskell is also immutable.


### The path to monads

In Haskell, a monad is a type class that is used to represent computations as abstract data types. Monads are used for handling side effects, such as I/O operations, exceptions, or state changes.

A Monad must be an instance of `Foldable`(optional), `Functor`, `Applicative`:
- The `Functor` type class is used for types that can be mapped over. A functor is a type constructor that implements the `fmap` function, which applies a function to a value in a context. The `fmap` function is used to apply a function to a value within a functor without changing the context. For example, if you have a `Maybe` value that contains an integer, you can use `fmap` to apply a function to the integer without removing it from the `Maybe` context.
- The `Applicative` type class is used for types that can be applied to values within a context. An applicative is a type constructor that implements the `pure` function and the `<*>` operator. The `pure` function is used to lift a value into a context, and the `<*>` operator is used to apply a function within a context to a value within a context. For example, if you have a `Maybe` value that contains a function and another `Maybe` value that contains an integer, you can use `<*>` to apply the function to the integer within the `Maybe` context.
- The `Foldable` type class is used for types that can be folded. A foldable is a type constructor that implements the `foldr` function, which is used to fold a structure of values, applying a function to each value and combining the results.
  However, it's important to note that while `Foldable` is not a requirement for a Monad but, many common Monads, like `Maybe`, `List`, and `IO`, are also instances of `Foldable`.

#### Foldable Class

**`Foldable`** is a class used for folding, if a class is foldable then we can apply `foldr` and `foldl`:
```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
```

A minimal implementation of `Foldable` requires `foldr`, `foldl` can be expressed in term of `foldr`:
```haskell
foldl f a bs = foldr (\b g x -> g (f x b)) id bs a
```
Where `id` is the identity function. The identity function is a function that always returns its argument unchanged. In the context of `foldl`, it is used as the initial accumulator value when the fold operation is applied to an empty list.

The opposite is not true, since `foldr` may work on infinite lists: in the presence of call-by-need evaluation, `foldr` will immediately return the application of `f` to the recursive case of folding over the rest of the list, if `f` is able to produce some part of its result without reference to the recursive case, then the recursion will stop, on the other hand, `foldl` will immediately call itself with new parameters until it reaches the end of the list.

We can easily define a `foldr` for binary trees:
```haskell
data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)

tfoldr f z Empty = z
tfoldr f z (Leaf x) = f x z
tfoldr f z (Node l r) = tfoldr f (tfoldr f z r) l

instance Foldable Tree where
  foldr = tfoldr

> foldr (+) 0 (Node (Node (Leaf 1) (Leaf 3)) (Leaf 5)) -- => 9
```

**`Maybe`** is used to represent computations that may fail: we either have `Just v` if we are lucky, or `Nothing` :
```haskell
data Maybe a = Nothing | Just a
```
`Maybe` is foldable:
```haskell
instance Foldable Maybe where
    foldr _ z Nothing = z
    foldr f z (Just x) = f x z
```


#### Functor Class

**`Functor`** is the class of all the types that offers a map operation within a context, the map operation of functors is called `fmap` and has type: 
```haskell
fmap :: (a -> b) -> f a -> f b
```

- `(a -> b)` means it needs a function that can turn a value of type `a` into a value of type `b`. This is a way to change one thing into another.
- `f a` represents a container that holds a value of type `a`. 
- `f b` represents a similar container, but it has a value of type `b`.

It is quite natural to define `fmap` for a container:
```haskell
data Maybe a = Nothing | Just a -- (conditional) container

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)
```

Well-defined functors should obey the following laws:
```haskell
fmap id = id -- (where id is the identity function)
fmap (f . g) = fmap f . fmap g -- (homomorphism)
```

Let's define a suitable `fmap` for trees:
```haskell
tmap f Empty = Empty
tmap f (Leaf x) = Leaf $ f x
tmap f (Node l r) = Node (tmap f l) (tmap f r)

instance Functor Tree where
    fmap = tmap
```


#### Applicative Class

In our course toward monads, we must consider also an extended version of
functors, i.e., **`Applicative`** functors:
```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```
In this definition, `f` is a type constructor, and `f a` is a `Functor` type. It's essential that `f` is parametric with one parameter.

Understanding the concept behind Applicative functors is simpler when you think of `f` as a *container*. Here's a breakdown:
1. The `pure` function takes a value and encapsulates it within the `f` container.
2. The `<*>` function, also known as "apply," is akin to `fmap`. However, instead of taking a regular function, it operates on an `f` container that holds a function. This function is then applied to a compatible container of the same kind.

Let's illustrate this with an example:
```haskell
instance Applicative Maybe where
    pure = Just
    Just f <*> m  = fmap f m
    Nothing <*> _ = Nothing
```
In this instance, we're working with the `Maybe` container. When you see `Just f <*> m`, it means we extract the function `f` from the `Just` container and apply it to the container `m`. If you encounter `Nothing` in this context, the result is always `Nothing`. This demonstrates the concept of applying functions within the `Maybe` container.

**Lists** are instances of `Foldable` and `Functor`, what about `Applicative`?
We introduce **`concat`**:
```haskell
concat :: Foldable t => t [a] -> [a]
```
It can be defined as:
```haskell
concat l = foldr (++) [] l
concat [[1,2],[3],[4,5]] -- => [1,2,3,4,5]
```
When we apply `concat` to a list of lists, it flattens them into a single list by concatenating their elements. By introducing the `concat` function and utilizing it with lists, we will be able to work with lists as instances of `Applicative`, allowing to perform various operations that involve combining and processing lists of elements.

Its composition with `map` is called **`concatMap`**:
```haskell
concatMap f l = concat $ map f l
> concatMap (\x -> [x, x+1]) [1,2,3]
[1,2,2,3,3,4]
```
`\x -> [x, x+1]` is a lambda function that takes an input argument `x` and returns a list containing two elements: `x` and `x+1`.

Now, when working with lists, we can construct a standard implementation of `<*>` for the `Applicative` type class:
```haskell
instance Applicative [] where
    pure x    = [x]
    fs <*> xs = concatMap (\f -> map f xs) fs
```
`pure` takes an element and encapsulates it within a list, while `<*>` applies a list of functions `fs` (`fs` is a container of functions) to a list of values `xs`.
`Applicative` is like a `Functor` but with more than a function:  we map the operations in sequence, then we concatenate the resulting lists.

Now we can apply list of operations to a list of values:
```haskell
> [(+1),(*2)] <*> [1,2,3]
[2,3,4,2,4,6]
```

Following the list approach, we can make our binary trees an instance of `Applicative` functors. To do this, we first define the concept of tree concatenation:
```haskell
tconc Empty t = t
tconc t Empty = t
tconc t1 t2 = Node t1 t2
```

Now, we can define `concat` and `concatMap` for trees (referred to as `tconcat` and `tconcmap`) similar to how we did for lists:
```haskell
tconcat t = tfoldr tconc Empty t
tconcmap f t = tconcat $ tmap f t

instance Applicative Tree where
pure = Leaf
fs <*> xs = tconcmap (\f -> tmap f xs) fs

> (Node (Leaf (+1))(Leaf (*2))) <*>
    Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
    
Node (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4))
    (Node (Node (Leaf 2) (Leaf 4)) (Leaf 6))
```

### Monads

Monads, in Haskell, are a type of algebraic data type that encapsulate computations, often referred to as _actions_. They enable chaining of these actions into an ordered sequence. Each action within this sequence is augmented with additional processing rules provided by the monad and executed automatically. It is essential for a `Monad` to be an instance of `Functor`, `Applicative`, and `Foldable` because these operations form the foundation of the monad concept.

Here is the Haskell definition of a Monad:
```haskell
class Applicative m => Monad m where
    -- Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)  :: m a -> (a -> m b) -> m b
    -- Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)   :: m a -> m b -> m b
    m >> k = m >>= \_ -> k
    -- Inject a value into the monadic type.
    return :: a -> m a
    return = pure
    -- Fail with a message.
    fail   :: String -> m a
    fail s = error s
```
In this definition, `>>=` and `>>`, also known as **bind** operators, are used to compose actions:
- `x >>= y` performs the computation `x`, takes the resulting value and passes it to `y` then performs `y`.
- `x >> y` is analogous, but "throws away" the value obtained by `x`

The `return` function is used to inject a value into the monadic type, while `fail` is used to handle errors. `return` is by default `pure`, so it is used to create a single monadic action, e.g., return 5 is an action containing the value 5.

A `Monad` instance is illustrated below using `Maybe`:
```haskell
instance Monad Maybe where
    (Just x) >>= k    = k x
    Nothing  >>= _    = Nothing
    fail _            = Nothing
```
The information managed automatically by the monad is the “bit” which encodes the success `Just`, or failure `Nothing` of the action sequence.

```haskell
Just 4 >> Just 5 >> Nothing >> Just 6 
```
Evaluates to `Nothing`, we have propagation of the error.

For a monad to behave correctly, method definitions must obey the following *laws*:

1. ***Identity Law***
   The Identity Law states that if we take a monadic value and use the `return` function to put it in a minimal context (i.e., the monad itself), then use the bind operator `>>=` to feed it to a function, it should be equivalent to just applying that function to the value: `return` is the **identity element**.
```haskell
return x >>= f  <=> f x -- left identity
m >>= return    <=> m   -- right identity
```
For example:
```haskell
> (return 4 :: Maybe Integer) >>= \x -> Just (x+1)
Just 5
> Just 5 >>= return
Just 5
```

2. ***Associativity Law***
   The Associativity Law states that the way in which monadic values are combined should not matter. This means that the order in which operations are performed should not affect the result: **associativity** for binds.
```haskell
(m >>= f) >>= g <=> m >>= (\x -> (f x >>= g))

> (return 4 >>= \x -> Just (x+1))
            >>= \x -> Just (x*2)
Just 10
> return 4 >>= (\y ->
                  ((\x -> Just (x+1)) y)
                >>= \x -> Just (x*2))
Just 10
```

The **`do`** syntax is used to avoid the explicit use of `>>=` and `>>`, translation:
```haskell
do e1 ; e2       <=> e1 >> e2
do p <- e1 ; e2  <=> e1 >>= \p -> e2
```

Others ways to express the `do` notation:
```haskell
do e1               do p <- e1
   e2                  e2
   
do { e1 ;           do { p <- e1;
     e2 }                e2 }
```

**`IO`** is a built-in monad in Haskell, we use the `do` notation to perform IO actions, though it looks like a sub-language its semantics are based on bind and pure, example:
```haskell
esp :: IO Integer
esp = do x <- return 4
         return (x+1)
> esp
5
```

#### Understanding the `do` Syntax

The `do` syntax is essentially syntactic sugar for chaining operations using the bind operator (`>>=`) and the sequencing operator (`>>`). Here's what each part means:

- `e1 >> e2`: This expression runs `e1` and then `e2`. The result of `e1` is discarded, and the result of `e2` is returned.
- `e1 >>= \p -> e2`: This expression binds the result of `e1` (a value in a monad) to `p` and then applies `e2` to `p`.

The `do` syntax abstracts away these details, allowing you to write code that looks more like traditional imperative programming.

> [!example] Example
Let's consider a simple example where we want to print a message followed by a prompt for user input:
> 
> ```haskell
> main :: IO ()
> main = do
>     putStrLn "Please enter your name:"
>     name <- getLine
>     putStrLn ("Hello, " ++ name)
> ```
> 
> This could be translated using `>>=` and `>>` as follows:
> 
> ```haskell
> main = putStrLn "Please enter your name:" >> getLine >>= \name -> putStrLn ("Hello, " ++ name)
> ```

In a **List**, monadic binding involves joining together a set of calculations for each value in the list, in practice *bind* is `concatMap`:
```haskell
instance Monad [] where
    xs >>= f = concatMap f xs
    fail _ = []
```
The underlying idea is to represent non-deterministic computations (each element (list) of the list is a possible computational path).

List comprehensions can be expressed in do or bind notation:
```haskell
[(x,y) | x <- [1,2,3], y <- [1,2,3]]

do x <- [1,2,3]
   y <- [1,2,3]
   return (x,y)
   
[1,2,3] >>= (\x -> [1,2,3] >>=
                   (\y ->
                     return (x,y)))
                     
concatMap f0 [1,2,3]
where f0 x = concatMap f1 [1,2,3]
             where f1 y = [(x,y)]                     
```

We can now to define our own monad with binary trees:
```haskell
instance Monad Tree where
    xs >>= f = tconcmap f xs
    fail _   = Empty
```


### State  Monad

We saw that monads are useful to automatically manage state (of the computation), we now define a general monad to do it (it is already available in the libraries as `Control.Monad.State`).

First of all, we define a type to represent our state: 
```haskell
data State st a = State (st -> (st, a))
```
The idea is having a type that represent a computation with a state, i.e., a function taking the current state and returning the next (type `a` is the explicit part of the monad).  
It is different from previous containers because we have two parameters, but we need unary type constructor so the "container" know has type constructor `State st`.

We need to make `State` an instance of `Functor`:
```haskell
instance Functor (State st) where
    fmap f (State g) = State (\s -> let (s’, x) = g s
                                    in (s’, f x))
```
The idea is quite simple: in a value of type `State st` a we apply `f` to the value of type `a`.

Then, we need to make `State` an instance of `Applicative`:
```haskell
instance Applicative (State st) where
    pure x = State (\t -> (t, x))
    (State f) <*> (State g) =
        State (\s0 -> let (s1, f’) = f s0
                          (s2, x) = g s1
                      in  (s2, f’ x))
```
The idea is similar to the previous one: we apply `f :: State st (a -> b)` to the data part of the monad.

The same approach can be used for the monad definition:
```haskell
instance Monad (State state) where
    State f >>= g = State (\olds ->
                            let (news, value) = f olds
                                State f’ = g value
                            in f’ news)
```
An important aspect of this monad is that monadic code does not get evaluated to data, but to a function, `State` is a function and bind is function composition.


---

[^memoized]: In computing, memoization or memoisation is an optimization technique used primarily to speed up computer programs by storing the results of expensive function calls and returning the cached result when the same inputs occur again

[^thunks]: In computer programming, a thunk is a subroutine used to inject a calculation into another subroutine. Thunks are primarily used to delay a calculation until its result is needed, or to insert operations at the beginning or end of the other subroutine.

[^ad-hoc-polymorphism]: In programming languages, ad hoc polymorphism is a kind of polymorphism in which polymorphic functions can be applied to arguments of different types, because a polymorphic function can denote a number of distinct and potentially heterogeneous implementations depending on the type of argument(s) to which it is applied. When applied to object-oriented or procedural concepts, it is also known as function overloading or operator overloading. 

[^Int]: Int has a fixed precision, while Integer is unlimited.

[^referentially-transparent]: In computer science, referential transparency and referential opacity are properties of parts of computer programs. An expression is called referentially transparent if it can be replaced with its corresponding value (and vice-versa) without changing the program's behavior. This requires that the expression be pure – its value must be the same for the same inputs and its evaluation must have no side effects. An expression that is not referentially transparent is called referentially opaque.