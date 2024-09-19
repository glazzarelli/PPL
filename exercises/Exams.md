## 2019-07-24 

### Scheme

Write a functional, tail recursive implementation of a procedure that takes a list of numbers $L$ and two values $x$ and $y$, and returns three lists: one containing all the elements that are less than both $x$ and $y$, the second one containing all the elements in the range $[x,y]$, the third one with all the elements bigger than both $x$ and $y$. It is not possible to use the named `let` construct in the implementation.

### Haskell

Consider a non-deterministic finite state automaton (NFSA) and assume that its states are values of a type `State` defined in some way. An NFSA is encoded in Haskell through three functions:
i) `transition :: Char → State → [State]`, i.e. the transition function.
ii) `end :: State → Bool`, i.e. a function stating if a state is an accepting state (True) or not.
iii) `start :: [State]`, which contains the list of starting states.

1) Define a data type suitable to encode the configuration of an NSFA.

2) Define the necessary functions (providing also all their types) that, given an automaton \( A \) (through `transition`, `end`, and `start`) and a string \( s \), can be used to check if \( A \) accepts \( s \) or not.

### Erlang

Define a master process which takes a list of nullary (or 0-arity) functions, and starts a worker process for each of them. The master must monitor all the workers and, if one fails for some reason, must re-start it to run the same code as before. The master ends when all the workers are done.

Note: for simplicity, you can use the library function `spawn_link/1`, which takes a lambda function, and spawns and links a process running it.

## 2020-09-03

### Scheme

Consider the following code:
```scheme
(define (a-function lst sep)
  (foldl (lambda (el next)
           (if (eq? el sep)
               (cons '() next)
               (cons (cons el (car next))
                     (cdr next))))
         (list '()) lst))
```
1. Describe what this function does; what is the result of the following call?
   ```scheme
   (a-function '(1 2 nop 3 4 nop 5 6 7 nop nop nop 9 9 9) 'nop)
   ```

2. Modify `a-function` so that in the example call the symbols `nop` are not discarded from the resulting list, which must also be reversed (of course, without using `reverse`).

Solution:


### Haskell

Consider the data structure `Tril`, which is a generic container consisting of three lists.
1. Give a data definition for `Tril`.
2. Define `list2tril`, a function which takes a list and 2 values `x` and `y`, say $x < y$, and builds a `Tril`, where the last component is the ending sublist of length `x`, and the middle component is the middle sublist of length $y - x$. Also, `list2tril L x y = list2tril L y x`.
   E.g. `list2tril [1,2,3,4,5,6] 1 3` should be a `Tril` with first component `[1,2,3]`, second component `[4,5]`, and third component `[6]`.
3. Make `Tril` an instance of `Functor` and `Foldable`.
4. Make `Tril` an instance of `Applicative`, knowing that the concatenation of 2 `Tril`s has first component which is the concatenation of the first two components of the first `Tril`, while the second component is the concatenation of the ending component of the first `Tril` and the beginning one of the second `Tril` (the third component should be clear at this point).

### Erlang

1. Define a `split` function, which takes a list and a number `n` and returns a pair of lists, where the first one is the prefix of the given list, and the second one is the suffix of the list of length `n`.
   E.g. `split([1,2,3,4,5], 2)` is `[{1,2,3}, {4,5}]`.

2. Using `split/2`, define a `splitmap` function which takes a function `f`, a list `L`, and a value `n`, and splits `L` with parameter `n`, then invokes two processes to `map f` on each one of the two lists resulting from the split. The function `splitmap` must return a pair with the two mapped lists.

Solution:
```erlang
-module(main).
-export([split/2, split_aux/3, split_map/3, mapl/3, start/0]).

split(L, N) ->
    split_aux([], L, N).

split_aux(Pre, Suff, N) ->
    case length(Suff) of
        LS when LS == N ->
            [Pre, Suff];
        _ ->
            [H|T] = Suff,
            split_aux((Pre++ [H]), T, N)
    end.

split_map(F, L, N) ->
    [Pre, Suff] = split(L, N),
    PrePid = spawn(?MODULE, mapl, [F, Pre, self()]),
    SuffPid = spawn(?MODULE, mapl, [F, Suff, self()]),
    P = receive {PrePid, PreResult} -> PreResult end,
    S = receive {SuffPid, SuffResult} -> SuffResult end,
    [P, S].         
    
mapl(F, L, P) ->
    P ! {self(), lists:map(F, L)}.

start() ->
    L = [1,2,3,4,5],
    io:format("~p~n", [split(L, 2)]),
    io:format("~p~n", [split_map((fun (X) -> X + 1 end), L , 2)]).
```


## 2020-01-15

### Scheme

Consider the `Foldable` and `Applicative` type classes in Haskell. We want to implement something analogous in Scheme for vectors. 
Note: you can use the following library functions in your code: `vector-map`, `vector-append`.

1. Define `vector-foldl` and `vector-foldr`.
2. Define `vector-pure` and `vector-<*>`.


Solution:
```scheme
(define (vector-foldl f z v)
  (define (vfoldl f z i l v)
    (if (eqv? i l)
        z
        (vfoldl f (f z (vector-ref v i)) (+ 1 i) l v)))
  (vfoldl f z 0 (vector-length v) v))


(define (vector-foldr f z v)
  (define (vfoldr f z i l v)
    (if (eqv? i l)
        z
        (f (vector-ref v i) (vfoldr f z (+ 1 i) l v))))
  (vfoldr f z 0 (vector-length v) v))


(define v (make-vector 5 1))

(vector-foldl + 0 v) ; 5
(vector-foldl - 0 v) ; -5

(vector-foldr + 0 v) ; 5
(vector-foldr - 0 v) ; 1

(define (vector-concat-map f v)
  (vector-foldr vector-append #() (vector-map f v)))

(define vector-pure vector)

(define (vector-<*> fs xs)
  (vector-concat-map (lambda (f) (vector-map f xs)) fs))

```

### Haskell

The following data structure represents a cash register. As it should be clear from the two accessor functions, the first component represents the current item, while the second component is used to store the price (not necessarily of the item: it could be used for the total).
```haskell
data CashRegister a = CashRegister { getReceipt :: (a, Float) } deriving (Show, Eq)

getCurrentItem = fst . getReceipt

getPrice = snd . getReceipt
```

1. Make `CashRegister` an instance of `Functor` and `Applicative`.
2. Make `CashRegister` an instance of `Monad`.

> [!note]- Haskell accessor function
`getReceipt` is a accessor function automatically generated by Haskell due to the record syntax used in defining the `CashRegister` data type. This function allows you to extract the receipt information from a `CashRegister` value.
> - `getReceipt :: CashRegister a -> (a, Float)`
> - This function extracts the tuple `(a, Float)` from a `CashRegister` value.
>
> So, if you have a `CashRegister` value, you can use `getReceipt` to extract its receipt information, like this:
> 
> ```haskell
> let register = CashRegister ("Item", 10.0)
> let receipt = getReceipt register
> ```
> 
> In this example, `receipt` will be `("Item", 10.0)`, assuming `"Item"` is of type `String` and `10.0` is of type `Float`.
> 
> The code is equivalent to:
> ```haskell
> data CashRegister a = CashRegister (a, Float) deriving (Show, Eq)
> 
> getCurrentItem :: CashRegister a -> a
> getCurrentItem (CashRegister (item, _)) = item
> 
> getPrice :: CashRegister a -> Float
> getPrice (CashRegister (_, price)) = price
> ```

> [!note]- Tuple `fst` and `snd`  
> ```haskell
> let myTuple = ("Hello", 42)
> let firstComponent = fst myTuple  -- This will be "Hello"
> let secondComponent = snd myTuple -- This will be 42
> ```

Solution:
```haskell
ffmap :: (a -> b) -> Float -> CashRegister a -> CashRegister b
ffmap f v reg = CashRegister(f (getCurrentItem reg),v + getPrice reg)

instance Functor CashRegister where
  fmap f  = ffmap f 0

instance Applicative CashRegister where
  pure x = CashRegister (x,0)
  freg <*> reg = ffmap (getCurrentItem freg) (getPrice reg) reg

instance Monad CashRegister where
  reg >>= f = (\ r -> let (item',value') = getReceipt (f (getCurrentItem reg))
                      in CashRegister(item', value' + getPrice reg)) reg
```

### Erlang

We want to implement something like Python’s `range` in Erlang, using processes:
```erlang
R = range(1,5,1)       % starting value, end value, step
next(R)                % is 1
next(R)                % is 2
...

next(R)                % is 5
next(R)                % is the atom stop_iteration
```

Define `range` and `next`, where `range` creates a process that manages the iteration, and `next` a function that talks with it, asking the current value.

Solution:
```erlang
ranger(Current, Stop, Inc) ->
    receive
        {Pid, next} ->
            if 
                Current =:= Stop ->
                    Pid ! stop_iteration;
                true ->
                    Pid ! Current,
                    ranger(Current + Inc, Stop, Inc)
            end
    end.

range(Start, Stop, Inc) ->
    spawn(?MODULE, ranger, [Start, Stop, Inc]).

next(Pid) ->
    Pid ! {self(), next},
    receive 
        V ->
            V
    end.
```

> [!note] Note
> If we are, for example, within the `start/0` process and we directly invoke `ranger` instead of spawning a new process using `range`, we would be stuck waiting for a message without the ability to call `next(R)`.
> ```erlang
> start() ->
>     R = ranger(1,5,1),      % starting value, end value, step
>     % Inside ranger, waiting for a message.
>     next(R),                % It returns 1
> 	...
> ```
> By spawning a new process, we return from the call within `range` to the starting process, allowing us to proceed further.

## 2020-02-07
### Scheme

Implement this new construct: `(each-until var in list until pred : body)`, where keywords are `each-until`, `in`, `until` and `:`. It works like a for-each with variable `var`, but it can end before finishing all the elements of `list` when the predicate `pred` on `var` becomes true.

E.g.
```scheme
(each-until x in '(1 2 3 4)
  until (> x 3) :
  (display (* x 3))
  (display " "))
```
shows on the screen: `3 6 9`

Solution:
```scheme
(define-syntax each-until
  (syntax-rules (in until :)
    [(_ var in list until pred : body ...)
     (call/cc (lambda (break)
                (let loop ((l list))
                  (when (null? l)
                    (break))
                  (let ((var (car list)))
                    (if (or pred (null? l))
                        (break)
                        (begin
                          body
                          ...))
                    (if (null? (cdr l))
                        (break)
                        (loop (cdr l)))))))]))
```

Alternative solution without `call/cc`:
```scheme
(define-syntax each-until
  (syntax-rules (in until :)
    ((_ x in L until pred : body ...)
     (let loop ((xs L))
       (unless (null? xs)
         (let ((x (car xs)))
           (unless pred
             (begin
               body ...
               (loop (cdr xs))))))))))
```

### Haskell

Consider a data type `PriceList` that represents a list of items, where each item is associated with a price, of type Float:
```haskell
data PriceList a = PriceList [(a, Float)]
```
1) Make `PriceList` an instance of Functor and Foldable.
2) Make `PriceList` an instance of Applicative, with the constraint that each application of a function in the left hand side of a `<*>` must increment a right hand side value’s price by the price associated with the function.

E.g.
```haskell
PriceList [(("nice "++), 0.5), (("good "++), 0.4)] <*>
PriceList [("pen", 4.5), ("pencil", 2.8), ("rubber", 0.8)]
```
is
```haskell
PriceList [("nice pen", 5.0),("nice pencil",3.3),("nice rubber",1.3),("good pen",4.9),
           ("good pencil",3.2),("good rubber",1.2)]
```

Solution:
```haskell
pmap :: (a -> b) -> Float -> PriceList a -> PriceList b
pmap f v (PriceList prices) = PriceList $ fmap (\x -> let (a, p) = x in (f a, p + v)) prices

instance Functor PriceList where
    fmap f prices = pmap f 0.0 prices

instance Foldable PriceList where
    foldr f i (PriceList prices) = foldr (\x y -> let (a, p) = x in f a y) i prices

(PriceList x) +.+ (PriceList y) = PriceList $ x ++ y

plconcat x = foldr (+.+) (PriceList []) x

instance Applicative PriceList where
    pure x = PriceList [(x, 0.0)]
    (PriceList fs) <*> xs = plconcat (fmap (\ff -> let (f, v) = ff in pmap f v xs) fs)
```

### Erlang

We want to create a simplified implementation of the “Reduce” part of the MapReduce paradigm. To this end, define a process `reduce_manager` that keeps track of a pool of reducers. When it is created, it stores a user-defined associative binary function ReduceF. It receives messages of the form `{reduce, Key, Value}`, and forwards them to a different “reducer” process for each key, which is created lazily (i.e. only when needed). Each reducer serves requests for a unique key. Reducers keep into an accumulator variable the result of the application of ReduceF to the values they receive. When they receive a new value, they apply ReduceF to the accumulator and the new value, updating the former. When the reduce_manager receives the message `print_results`, it makes all its reducers print their key and incremental result.

For example, the following code (where the meaning of `string:split` should be clear from the context):
```erlang
word_count(Text) ->
    RMPid = start_reduce_mgr(fun (X, Y) -> X + Y end),
    lists:foreach(fun (Word) -> RMPid ! {reduce, Word, 1} end, string:split(Text, " ", all)),
    RMPid ! print_results,
    ok.
```
causes the following print:
```
1> mapreduce:word_count("sopra la panca la capra campa sotto la panca la capra crepa").
sopra: 1
la: 4
panca: 2
capra: 2
campa: 1
sotto: 1
crepa: 1
ok
```

Solution:
```erlang
start_reduce_mgr(ReduceF) ->
    spawn(?MODULE, reduce_mgr, [ReduceF, #{}]).

reduce_mgr(ReduceF, Reducers) ->
    receive
        print_results ->
            lists:foreach(fun ({_, RPid}) -> RPid ! print_results end, maps:to_list(Reducers));
        {reduce, Key, Value} ->
            case Reducers of
                #{Key := RPid} ->
                    RPid ! {Key, Value},
                    reduce_mgr(ReduceF, Reducers);
                _ ->
                    NewReducer = spawn(?MODULE, reducer, [ReduceF, Key, Value]),
                    reduce_mgr(ReduceF, Reducers#{Key => NewReducer})
            end
    end.

reducer(ReduceF, Key, Result) ->
    receive
        print_results ->
            io:format("~s: ~w~n", [Key, Result]);
        {Key, Value} ->
            reducer(ReduceF, Key, ReduceF(Result, Value))
    end.

```


## 2021-06-22

### Scheme

Define a function `mix` which takes a variable number of arguments $x_0, x_1, x_2, \ldots, x_n$, the first one a function, and returns the list $(x_1 (x_2 \ldots (x_0(x_1) x_0(x_2) \ldots x_0(x_n)) x_n) \ldots x_i)$.

E.g.
```scheme
(mix (lambda (x) (* x x)) 1 2 3 4 5)
```
returns: `'(1 (2 (3 (4 (5 (1 4 9 16 25) 5) 4) 3) 2) 1)`

**Idea**: for each argument passed evaluate `((car args) (recursive call) (car args))` and then when as the last step of the recursive call do `(map f args)`
Problems:
- how to access the (original) `args` in the last recursive call
- how to achieve recursion with [[Guess the output#Variable length arguments|variable length arguments]]
Both problems can be solved using a auxiliary function that treats the arguments as a list and has the original `args` "stored" like an accumulator:
```scheme
(define (mix f . args)
  (define (mix-aux last f args)
    (if (null? args)
        (map f last)
        (list (car args) (mix-aux last f (cdr args)) (car args))))
  (mix-aux args f args))
```

A shorter solution using `foldr`:
```scheme
(define (mix g . L)
  (foldr (lambda (x y) (list x y x)) (map g L) L))
```

### Haskell

Define a data-type called `BTT` which implements trees that can be binary or ternary (or mixed), and where every node contains a value, but the empty tree (Nil). Note: there must not be unary nodes, like leaves.
1. Make `BTT` an instance of Functor and Foldable.
2. Define a concatenation for `BTT`, with the following constraints:
   - If one of the operands is a binary node, such node must become ternary, and the other operand will become the added subtree (e.g. if the binary node is the left operand, the rightmost node of the new ternary node will be the right operand).
   - If both the operands are ternary nodes, the right operand must be appended on the right of the left operand, by recursively calling concatenation.
3. Make `BTT` an instance of Applicative.

```haskell
data BTT a = BT a (BTT a) (BTT a) | TT a (BTT a) (BTT a) (BTT a) | Nil

bleaf x = BT x Nil Nil
tleaf x = TT x Nil Nil Nil

instance Functor BTT where
  fmap _ Nil = Nil
  fmap f (BT x left right) = BT (f x) (fmap f left) (fmap f right)
  fmap f (TT x left center right) = TT (f x) (fmap f left) (fmap f center) (fmap f right)

instance Foldable BTT where
  foldr f z Nil = z
  foldr f z (BT x left right) = f x (foldr f (foldr f z right) left)
  foldr f z (TT x left center right) = f x (foldr f (foldr f (foldr f z right) center) left)

concatbtt :: BTT a -> BTT a -> BTT a
concatbtt tree Nil = tree
concatbtt Nil tree = tree 
concatbtt (BT x left right) tree = TT x left right tree
concatbtt tree (BT x left right) = TT x left right tree
concatbtt (TT x l c r) tree = TT x l c (concatbtt r tree)

instance Applicative BTT where
  pure = bleaf
  ftree <*> tree = foldr concatbtt Nil (fmap (\ f -> fmap f tree) ftree)

atree :: BTT Int
atree = TT 10 
            (BT 5 
                (bleaf 3) 
                (bleaf 7)) 
            (TT 15 
                (bleaf 13) 
                (bleaf 17) 
                Nil) 
            (BT 20 
                Nil 
                (TT 25 
                    Nil 
                    Nil 
                    (bleaf 30)))
```

### Erlang

Create a function `node_wait` that implements nodes in a tree-like topology. Each node, which is a separate agent, keeps track of its parent and children (which can be zero or more), and contains a value. An integer weight is associated to each edge between parent and child.

A node waits for two kinds of messages:
- `{register_child, ...}`, which adds a new child to the node (replace the dots with appropriate values).
- `{get_distance, Value}`, which causes the recipient to search for `Value` among its children, by interacting with them through appropriate messages. When the value is found, the recipient answers with a message containing the minimum distance between it and the node containing "Value", considering the sum of the weights of the edges to be traversed. If the value is not found, the recipient answers with an appropriate message. While a node is searching for a value among its children, it may not accept any new children registrations. E.g., if we send `{get_distance, a}` to the root process, it answers with the minimum distance between the root and the closest node containing the atom `a` (which is 0 if `a` is in the root).

## 2021-07-14

### Scheme

- Define a `defun` construct like in Common Lisp, where `(defun f (x1 x2 ...) body) `is used for defining a function f with parameters x1 x2 ....
  Every function defined in this way should also be able to return a value x by calling (ret x).

The provided Scheme code defines a custom `defun` macro that provides a way to define functions with early returns. Here's a breakdown of the code:
1. `(define ret-store '())`: This line initializes an empty list `ret-store`. This list will store the continuations of the functions defined using `defun`.
2. `(define (ret v) ((car ret-store) v))`: This function takes a value `v` and applies the first continuation in `ret-store` to it. This effectively simulates an early return from the function currently being executed.
3. The `defun` macro is defined using `syntax-rules`, a built-in macro system in Scheme. This macro transforms the code at compile time.

  ```scheme
  (define-syntax defun
    (syntax-rules ()
      ((_ fname (var ...) body ...)
       (define (fname var ...)
         (let ((out (call/cc (lambda (c)
                              (set! ret-store (cons c ret-store))
                              body ...))))
           (set! ret-store (cdr ret-store))
           out)))))
  ```

  The `defun` macro takes three arguments: `fname` (the function name), `var...` (the function parameters), and `body...` (the function body). It defines a new function with the given name and parameters. Inside this function, it uses `call/cc` (call with current continuation) to capture the current continuation `c` and adds it to the beginning of `ret-store`. Then it executes the function body. After the body execution, it removes the captured continuation from `ret-store` and returns the result of the body execution.

In summary, this code provides a way to define functions in Scheme that can perform early returns using the `ret` function. The `defun` macro captures the current continuation at the start of the function and stores it in `ret-store`, allowing `ret` to jump back to the saved continuation at any point in the function.

```scheme
(define ret-store '())
(define (ret v)
  ((car ret-store) v))
(define-syntax defun
  (syntax-rules ()
    ((_ fname (var ...) body ...)
     (define (fname var ...)
       (let ((out (call/cc (lambda (c)
                             (set! ret-store (cons c ret-store))
                             body ...))))
         (set! ret-store (cdr ret-store))
out)))))

;; Define the factorial function using our `defun` macro
(defun factorial (n)
  ;; If n is less than 0, return early with `ret`
  (if (< n 0)
      (ret "Error: n must be non-negative")
      (if (= n 0)
          1
          (* n (factorial (- n 1))))))

;; Test the function with a positive number
(displayln (factorial 5))  ; Outputs: 120

;; Test the function with a negative number
(displayln (factorial -5)) ; Outputs: "Error: n must be non-negative"
```

The line `(ret "Error: n must be non-negative")` in the provided Racket code is using the `ret` function to return from the enclosing function (defined by the `defun` macro) with a specific value, in this case, the string "Error: n must be non-negative".

Here's how this works in detail:
1. `ret` is a function that takes a single argument `v` and applies the first element of `ret-store` to `v`. The first element of `ret-store` is a continuation, which is a kind of function that represents the rest of the computation from a certain point in the program.
2. In the `defun` macro, `call/cc` is used to capture the current continuation (which represents the rest of the computation from the point where `call/cc` is called) and store it in `ret-store`. This continuation represents the point to return to when `ret` is called.
3. When `(ret "Error: n must be non-negative")` is called, it applies the stored continuation to the string "Error: n must be non-negative". This effectively "returns" from the enclosing function, jumping back to the point where the continuation was captured, and providing the string "Error: n must be non-negative" as the result of the `call/cc` expression.
4. The `defun` macro then completes with this result, effectively returning the error message from the function that was defined with `defun`.

This use of `ret` and `defun` provides a way to implement non-local exits in Racket: the ability to return from a function from deep within nested expressions.

### Haskell

1) Define a "generalized" zip function which takes a finite list of possibly infinite lists, and returns a possibly infinite list containing a list of all the first elements, followed by a list of all the second elements, and so on.
E.g. `gzip [[1,2,3],[4,5,6],[7,8,9,10]] ==> [[1,4,7],[2,5,8],[3,6,9]]`

2) Given an input like in 1), define a function which returns the possibly infinite list of the sum of the two greatest elements in the same positions of the lists.
	E.g. `sum_two_greatest [[1,8,3],[4,5,6],[7,8,9],[10,2,3]] ==> [17,16,15]`

***Idea:*** `gzip` should *for-each* sublist take the first element and make it a list, then take the second element and do the same etc. and then results should be concatenated.
In the first iteration something like this should happen:
```haskell
takefirst list =  [fmap head list]
takefirst [[1,2,3],[4,5,6],[7,8,9,10]] -- [[1,4,7]]
```
Now the next step is to use this recursively with the tail of each list, how to call again `takefirst` with the tail of each list?
```haskell
gzip :: [[a]] -> [[a]]
gzip [[]] = [[]]
gzip list = [fmap head list] ++ gzip (fmap tail list)
```
The problem now is that with different length sublists this does not work since `tail` cannot be called on an empty list. 
We can add a guard that check using `filter` to see if there are not empty sublist otherwise end the recursion:
```haskell
gzip :: Eq a => [[a]] -> [[a]]
gzip [[]] = [[]]
gzip list
  | length (filter (\ l -> l == []) list) == 0 = [fmap head list] ++ gzip (fmap tail list)
  | otherwise = []
```
Observations:
- `(\ l -> l == [])` can be rewritten as `null` this let us remove the type class `Eq a` from the definition to avoid compilation errors.
- `lenght l == 0` is equal to `null l`.
- applied to a predicate and a list, `any` determines if any element of the list satisfies the predicate.

Solution:
```haskell
gzip :: [[a]] -> [[a]]
gzip list
  | any null list = []
  | otherwise = map head list : gzip (map tail list)

store_two_greatest v (x,y) | v > x = (v,y) 
store_two_greatest v (x,y) | x >= v && v > y = (x,v) 
store_two_greatest v (x,y) = (x,y) 

two_greatest :: Ord a => [a] -> (a, a)
two_greatest (x:[]) = (x, x)
two_greatest (x:y:xs) = foldr store_two_greatest (if x > y then (x, y) else (y, x)) xs
two_greatest _ = error "List must have at least two elements"

sum_two_greatest xs = [let (x,y) = two_greatest v in x+y | v <- gzip xs]
```

### Erlang

Consider a main process which takes two lists: one of function names, and one of lists of parameters (the first element of with contains the parameters for the first function, and so forth). For each function, the main process must spawn a worker process, passing to it the corresponding argument list. If one of the workers fails for some reason, the main process must create another worker running the same function. The main process ends when all the workers are done.

```erlang
listmlink([], [], Pids) -> Pids;
listmlink([F|Fs], [D|Ds], Pids) ->

    Pid = spawn_link(?MODULE, F, D),
    listmlink(Fs, Ds, Pids#{Pid => {F,D}}).

master(Functions, Arguments) ->
    process_flag(trap_exit, true),
    Workers = listmlink(Functions, Arguments, #{}),
    master_loop(Workers, length(Functions)).

master_loop(Workers, Count) ->
    receive

        {'EXIT', _, normal} ->
            if

                Count =:= 1 -> ok;

                true -> master_loop(Workers, Count-1)
            end;

        {'EXIT', Child, _} ->
            #{Child := {F,D}} = Workers,
            Pid = spawn_link(?MODULE, F, D),
            master_loop(Workers#{Pid => {F,D}}, Count)

end.
```

>[!help] Explanation
> This Erlang code is an example of a simple supervisor model where a master process spawns a number of worker processes, monitors them, and restarts them if they fail. 
> 
> Let's break it down:
> 
> 1. `listmlink/3` function:
> 
> This function takes three arguments: two lists (`[F|Fs]` and `[D|Ds]`) and a map (`Pids`). It uses pattern matching to destructure the first two arguments into a head and a tail (F and Fs for the first list, D and Ds for the second). 
> 
> ```erlang
> listmlink([], [], Pids) -> Pids;
> listmlink([F|Fs], [D|Ds], Pids) ->
>     Pid = spawn_link(?MODULE, F, D),
>     listmlink(Fs, Ds, Pids#{Pid => {F,D}}).
> ```
> 
> The function `spawn_link/3` creates a new process that is linked to the current one, and starts it with a function `F` from the current module (`?MODULE`) with `D` as an argument. The process ID of the newly created process is stored in `Pid`.
> 
> Then, the function calls itself recursively, passing the tails of the lists and a map that associates the Pid of the spawned process to the function and its argument. This continues until the lists are empty, at which point it returns the map of Pids.
> 
> 2. `master/2` function:
> 
> This function starts the supervisor process. It sets a process flag to trap exits, meaning it will receive a message when a linked process terminates instead of terminating itself.
> 
> ```erlang
> master(Functions, Arguments) ->
>     process_flag(trap_exit, true),
>     Workers = listmlink(Functions, Arguments, #{}),
>     master_loop(Workers, length(Functions)).
> ```
> 
> It then calls `listmlink/3` to spawn and link the worker processes, and starts the supervisor loop with the map of workers and the number of workers.
> 
> 3. `master_loop/2` function:
> 
> This function implements the supervisor loop. It waits for exit messages from worker processes and restarts them if necessary.
> 
> ```erlang
> master_loop(Workers, Count) ->
>     receive
>         {'EXIT', _, normal} ->
>             if
>                 Count =:= 1 -> ok;
>                 true -> master_loop(Workers, Count-1)
>             end;
>         {'EXIT', Child, _} ->
>             #{Child := {F,D}} = Workers,
>             Pid = spawn_link(?MODULE, F, D),
>             master_loop(Workers#{Pid => {F,D}}, Count)
>     end.
> ```
> 
> If a worker exits normally and it's the last one, it simply returns `ok`. If there are more workers, it subtracts one from the count and recurses.
> 
> If a worker exits for any reason other than `normal`, it pulls the function and argument associated with that worker from the map, respawns the worker with `spawn_link/3`, and recurses with the updated map and count.
> 
> Overall, this code demonstrates how to use Erlang's process linking and message passing features to implement a basic supervision tree.

In Erlang, a map is a collection of key-value pairs. There are two main types of maps: small maps and large maps. A small map contains at most 32 elements, while a large map contains more than 32 elements [Source 0](https://www.erlang.org/doc/efficiency_guide/maps).

Small maps have a compact representation, making them suitable as an alternative to records. Large maps are represented as a tree structure that can be efficiently searched and updated regardless of the number of elements.

Here are some guidelines for using maps:

- Avoid having more than 32 elements in the map. Once there are more than 32 elements, the map requires more memory and keys cannot be shared with other instances of the map.
- Always create a new map with all keys that will ever be used. This helps to maximize sharing of keys and minimize memory use.
- Use the `:=` operator when updating the map. This operator is slightly more efficient and helps catch misspelled keys.
- Match and update multiple map elements at once whenever possible.
- Avoid default values and the `maps:get/3` function. These can lead to less effective sharing of keys between different instances of the map.

Here is an example of creating a map and updating it:

```erlang
% Create a map
Map1 = #{x => 1, y => 2, z => 3}.

% Update the map
Map = Map1#{x := 4, y := 5, z := 6}.
```

In this example, `Map1` is a small map with three elements. `Map` is the updated version of `Map1`, with the values of `x`, `y`, and `z` changed.

Maps also provide several useful functions, such as `maps:find/2`, `maps:get/2`, `maps:put/3`, `maps:update/3`, `maps:remove/2`, `maps:take/2`, `maps:values/1`, `maps:keys/1`, `maps:size/1`, `maps:fold/3`, `maps:map/2`, `maps:filter/2`, `maps:filtermap/2`, `maps:merge/2`, `maps:merge_with/3`, `maps:iterator/1`, `maps:next/1`, `maps:to_list/1`, `maps:from_list/1`, `maps:from_keys/2`, `maps:with/2`, and `maps:without/2`.

In Erlang, a record is a data structure that allows you to group a fixed number of items together. Each item is associated with a unique identifier, called a field name. Unlike arrays and tuples, the items in a record can be accessed by their names, not by their positions [Source 1](https://learnyousomeerlang.com/maps).

Here is an example of defining a record:

```erlang
-record(person, {name, age}).
```

This defines a record named `person` with two fields: `name` and `age`.

You can create an instance of a record like this:

```erlang
John = #person{name="John", age=30}.
```

In this example, `John` is a record of type `person` with `name` set to `"John"` and `age` set to `30` [Source 1](https://learnyousomeerlang.com/maps).

Comparing records and maps in Erlang, here are the key differences:

- **Memory Usage**: Records will use slightly less memory than maps [Source 0](https://www.erlang.org/doc/efficiency_guide/maps).
- **Performance**: Performance is expected to be slightly better with records in most circumstances [Source 0](https://www.erlang.org/doc/efficiency_guide/maps).
- **Field Names**: If the name of a record field is misspelled, there will be a compilation error. If a map key is misspelled, the compiler will give no warning and the program will fail in some way when it is run [Source 0](https://www.erlang.org/doc/efficiency_guide/maps).
- **Recompilation**: The disadvantage of records compared to maps is that if a new field is added to a record, all code that uses that record must be recompiled. Because of that, it is recommended to only use records within a unit of code that can easily be recompiled all at once, for example within a single application or single module [Source 0](https://www.erlang.org/doc/efficiency_guide/maps).

However, maps are more flexible and can be used as an alternative to records in certain scenarios. For example, when you need to handle complex nested key/value data structures that would frequently cross module boundaries, maps will be more helpful. Also, maps support pattern matching, which can be more convenient and expressive than the pattern matching available with records [Source 1](https://learnyousomeerlang.com/maps).


## 2021-08-31

### Scheme

1. Define a procedure which takes a natural number `n` and a default value, and creates a `n` by `n` matrix filled with the default value, implemented through vectors (i.e. a vector of vectors).
2. Let `S = {0, 1, ..., n-1} x {0, 1, ..., n-1}` for a natural number `n`. Consider a `n` by `n` matrix `M`, stored in a vector of vectors, containing pairs `(x,y) ∈ S`, as a function from `S` to `S` (e.g. `f(2,3) = (1,0)` is represented by `M[2][3] = (1,0)`). Define a procedure to check if `M` defines a bijection (i.e. a function that is both injective and surjective).

Solution:
```scheme
```


### Haskell

Consider a `Slist` data structure for lists that store their length. Define the `Slist` data structure, and make it an instance of `Foldable`, `Functor`, `Applicative` and `Monad`.


Solution:
```haskell
data Slist a = Slist Int [a] deriving (Show, Eq)
makeSlist v = Slist (length v) v
instance Foldable Slist where
  foldr f i (Slist n xs) = foldr f i xs
instance Functor Slist where
  fmap f (Slist n xs) = Slist n (fmap f xs)
instance Applicative Slist where
  pure v = Slist 1 (pure v)
  (Slist x fs) <*> (Slist y xs) = Slist (x*y) (fs <*> xs)
instance Monad Slist where
  fail _ = Slist 0 []
  (Slist n xs) >>= f = makeSlist (xs >>= (\x -> let Slist n xs = f x in xs))
```


### Erlang

Define a function which takes two list of PIDs $[x_1, x_2, ...]$, $[y_1, y_2, ...]$, having the same length, and a function `f`, and creates a different "broker" process for managing the interaction between each pair of processes $x_i$ and $y_i$.  
At start, the broker process $i$ must send its PID to $x_i$ and $y_i$ with a message `{broker, PID}`. Then, the broker $i$ will receive messages `{from, PID, data, D}` from $xi$ or $yi$, and it must send to the other one an analogous message, but with the broker PID and data D modified by applying `f` to it.

A special stop message can be sent to a broker $i$, that will end its activity sending the same message to $x_i$ and $y_i$.


Solution:
```erlang
broker(X, Y, F) ->
    X ! {broker, self()},
    Y ! {broker, self()},
    receive
        {from, X, data, D} ->
            Y ! {from, self(), data, F(D)},
            broker(X, Y, F);
        {from, Y, data, D} ->
            X ! {from, self(), data, F(D)},
            broker(X, Y, F);
        stop ->
            X ! stop,
            Y ! stop,
            ok
    end.

twins([], _, _) -> ok;
twins([X|Xs], [Y|Ys], F) ->
    spawn(?MODULE, broker, [X, Y, F]),
    twins(Xs, Ys, F).
```


## 2022-01-21

### Scheme

Define a new construct called `block-then` which creates two scopes for variables, declared after the scopes, with two different bindings. E.g., the evaluation of the following code:
```scheme
(block-then
  ((displayln (+ x y))
   (displayln (+ x y z)))
  ((displayln (+ x z))
   (displayln (+ x y z)))
  where ((x <- 12 3) (y <- 8 7) (z <- 3 22))
)
```

should show on the screen:
```
20
96
10
6
```

Solution:
```scheme
(define-syntax block
  (syntax-rules (where then <-)
    ((_ (e1 ...) then (e2 ...) where (v <- a b) ...)
     (begin
       (let ((v a) ...)
         e1 ...)
       (let ((v b) ...)
         e2 ...)))))
```

### Haskell

Consider a `Tvll` (two-values/two-lists) data structure, which can store either two values of a given type, or two lists of the same type.

Define the `Tvll` data structure, and make it an instance of Functor, Foldable, and Applicative.

Solution:
```haskell
data Tvtl a = Tv a a | Tl [a] [a]
    deriving (Show, Eq)

instance Functor Tvtl where
    fmap f (Tv x y) = Tv (f x) (f y)
    fmap f (Tl x y) = Tl (fmap f x) (fmap f y)

instance Foldable Tvtl where
    foldr f i (Tv x y) = f x (f y i)
    foldr f i (Tl x y) = foldr f (foldr f i y) x

(+++) :: Tvtl a -> Tvtl a -> Tvtl a
(Tv x y) +++ (Tv z w) = Tl [x,y] [y,w]
(Tv x y) +++ (Tl l r) = Tl (x:l) (y:r)
(Tl l r) +++ (Tv x y) = Tl (l++[x]) (r++[y])
(Tl l r) +++ (Tl x y) = Tl (l++x) (r++y)

tvtlconcat :: Tvtl (Tvtl a) -> Tvtl a
tvtlconcat t = foldr (+++) (Tl [] []) t

tvtlcmap :: (a -> b) -> Tvtl a -> Tvtl b
tvtlcmap f t = tvtlconcat $ fmap f t

instance Applicative Tvtl where
    pure x = Tl [x] []
    x <*> y = tvtlcmap (\f -> fmap f y) x

```

### Erlang

Create a distributed hash table with **separate chaining**. The hash table will consist of an agent for each bucket, and a master agent that stores the buckets' PIDs and acts as a middleware between them and the user. Actual key/value pairs are stored in the bucket agents.

The middleware agent must be implemented by a function called `hashtable_spawn` that takes as its arguments (1) the hash function and (2) the number of buckets. When executed, `hashtable_spawn` spawns the bucket nodes, and starts listening for queries for them. Such queries can be of two kinds:

- **Insert**: `{insert, Key, Value}` inserts a new element into the hash table, or updates it if an element with the same key exists.
- **Lookup**: `{lookup, Key, RecipientPid}` sends to the agent with PID `RecipientPid` a message of the form `{found, Value}`, where `Value` is the value associated with the given key, if any. If no such value exists, it sends the message `not_found`.

The following code:
```erlang
main() ->
  HT = spawn(?MODULE, hashtable_spawn, [fun(Key) -> Key rem 7 end, 7]),
  HT ! {insert, 5, "Apple"},
  HT ! {insert, 8, "Orange"},
  timer:sleep(888),
  HT ! {lookup, 8, self()},
  receive
    {found, A1} -> io:format("~s~n", [A1])
  end,
  HT ! {insert, 8, "Pineapple"},
  timer:sleep(888),
  HT ! {lookup, 8, self()},
  receive
    {found, A2} -> io:format("~s~n", [A2])
  end.
```

should print the following:
```
Orange
Pineapple
```

Solution:
```erlang
hashtable_spawn(HashFun, NBuckets) ->
    BucketPids = [spawn(?MODULE, bucket, [[]]) || _ <- lists:seq(0, NBuckets)],
    hashtable_loop(HashFun, BucketPids).

hashtable_loop(HashFun, BucketPids) ->
    receive
        {insert, Key, Value} ->
            lists:nth(HashFun(Key) + 1, BucketPids) ! {insert, Key, Value},
            hashtable_loop(HashFun, BucketPids);
        {lookup, Key, AnswerPid} ->
            lists:nth(HashFun(Key) + 1, BucketPids) ! {lookup, Key, AnswerPid},
            hashtable_loop(HashFun, BucketPids)
    end.

bucket(Content) ->
    receive
        {insert, Key, Value} ->
            NewContent = lists:keystore(Key, 1, Content, {Key, Value}),
            bucket(NewContent);
        {lookup, Key, AnswerPid} ->
            case lists:keyfind(Key, 1, Content) of
                false ->
                    AnswerPid ! not_found;
                {_, Value} ->
                    AnswerPid ! {found, Value}
            end,
            bucket(Content)
    end.

%% You may replace calls to lists:keystore/4 and lists:keyfind/3 with calls to the following functions:

keystore_first(Key, [{TupleKey, _} | TupleTail], NewValue) when Key == TupleKey ->
    [{Key, NewValue} | TupleTail];
keystore_first(Key, [Tuple | TupleTail], NewValue) ->
    [Tuple | keystore_first(Key, TupleTail, NewValue)];
keystore_first(Key, [], NewValue) ->
    [{Key, NewValue}].

keyfind_first(Key, [{TupleKey, TupleValue} | _]) when Key == TupleKey ->
    {TupleKey, TupleValue};
keyfind_first(Key, [_ | TupleTail]) ->
    keyfind_first(Key, TupleTail);
keyfind_first(_, []) ->
    false.
```

## 2022-02-10

### Scheme

Consider the following code:
```scheme
(define (r x y . s)
	  (set! s (if s (car s) 1))
  (lambda ()
    (if (< x y)
        (let ((z x))
          (set! x (+ s x))
          z) y)))
```

1. What can we use `r` for? Describe how it works and give some useful examples of its usage.
2. It makes sense to create a version of `r` without the `y` parameter? If the answer is yes, implement such version; if no, explain why.

The given code defines a Scheme function `r` that essentially creates a generator for producing a sequence of numbers. This function takes at least two arguments `x` and `y`, and an optional argument `s`. When the optional argument `s` is provided, it sets `s` to the first element of the list `s`; otherwise, it defaults `s` to 1. The function returns a lambda (an anonymous function) that, when called, will return the next number in the sequence starting from `x` and increasing by `s` until `x` is not less than `y`.

Here's the breakdown of how the function `r` works:

1. The function `r` is defined with a minimum of two required parameters `x` and `y`, and a rest parameter `s` which can collect additional arguments into a list.
2. The `set!` form is used to mutate the variable `s`. If `s` is provided (non-empty), it takes the first value from the list `s` using `car`; otherwise, `s` is set to 1.
3. It returns a lambda (anonymous function) that captures the variables `x`, `y`, and `s` in its closure.
4. Each time the lambda is called, it checks if `x` is less than `y`.
   - If `x` is less than `y`, it captures the current value of `x` in a local variable `z`, increments `x` by `s`, and then returns `z`.
   - If `x` is not less than `y`, it simply returns `y`.

Here are some examples of how the function `r` can be used:

```scheme
(define my-range (r 1 10 2)) ; Create a range generator starting at 1, ending at 10, stepping by 2.
(my-range) ; => 1
(my-range) ; => 3
(my-range) ; => 5
(my-range) ; => 7
(my-range) ; => 9
(my-range) ; => 10 (since the next value 11 would be greater than y)

(define my-counter (r 0 100)) ; Create a counter starting at 0, ending at 100, stepping by default value 1.
(my-counter) ; => 0
(my-counter) ; => 1
(my-counter) ; => 2
; ... and so on until it returns 100 for each subsequent call.
```

In the first example, `my-range` is a generator that will produce numbers starting from 1 and increasing by 2 each time `my-range` is called until it reaches or surpasses 10. In the second example, `my-counter` is a simple counter that starts at 0 and increments by 1 up to 100.

This kind of generator can be useful for creating custom iterators where you want to generate a sequence of numbers on-the-fly without storing the entire sequence in memory. It can be used in various contexts, such as iterating over a range with a specific step size, creating custom counters, or generating values lazily.


### Haskell

Consider a data structure `Gtree` for general trees, i.e. trees containing some data in each node, and a variable number of children.
1. Define the `Gtree` data structure.
2. Define `gtree2list`, i.e. a function which translates a `Gtree` to a list.
3. Make `Gtree` an instance of `Functor`, `Foldable`, and `Applicative`.

```haskell
module Main where

data GTree a = TNil | GTree a [GTree a] 

gleaf v = GTree v [TNil]

gtree2list TNil = []
gtree2list (GTree v trees) = v : concatMap gtree2list trees

gmap f TNil = TNil
gmap f (GTree v trees) = GTree (f v) (map (gmap f) trees)

instance Show a => Show (GTree a) where
  show TNil = "TNil"
  show (GTree v trees) = "GTree " ++ show v ++ " " ++ show trees

instance Functor GTree where
  fmap = gmap

instance Foldable GTree where
  foldr f z t = foldr f z (gtree2list t)

t +++ TNil = t
TNil +++ t = t
(GTree x ts) +++ t = GTree x (ts ++ [t])

concattg :: Foldable t => t (GTree a) -> GTree a
concattg = foldr (+++) TNil

t1 :: GTree Int
t1 = GTree 1 [gleaf 2, GTree 3 [gleaf 4, gleaf 5, gleaf 6], GTree 7 [gleaf 8]]

t2 :: GTree (Int -> Int)
t2 = GTree (+ 1) [gleaf (1-), gleaf (* 2)]


instance Applicative GTree where
  pure = gleaf
  fs <*> ts = concattg $ fmap (\f -> fmap f ts) fs
  
main = do
  print (gtree2list t1)
  print (gtree2list (gmap (+1) t1))
  print (sum t1)
  print (gtree2list (t2 <*> t1))
```

### Erlang

Define a parallel lexer, which takes as input a string x and a chunk size n, and translates all the words in the strings to atoms, sending to each worker a chunk of x of size n (the last chunk could be shorter than n). You can assume that the words in the string are separated only by space characters (they can be more than one - the ASCII code for ' ' is 32); it is ok also to split words, if they overlap on different chunks. E.g.

plex("this is a nice test", 6) returns [[this,i],[s,a,ni],[ce,te],[st]] For you convenience, you can use the library functions:

• `lists:sublist(List, Position, Size)` which returns the sublist of List of size Size from position Position (starting at 1);
• `list_to_atom(Word)` which translates the string Word into an atom.

```erlang
split(List, Size, Pos, End) when Pos < End ->
    [lists:sublist(List, Pos, Size)] ++ split(List, Size, Pos+Size, End);

split(_, _, _, _) -> [].

lex([X|Xs], []) when X =:= 32 -> % 32 is ' '
    lex(Xs, []);

lex([X|Xs], Word) when X =:= 32 ->
    [list_to_atom(Word)] ++ lex(Xs, []);

lex([X|Xs], Word) ->
    lex(Xs, Word++[X]);

lex([], []) ->
    [];

lex([], Word) ->
    [list_to_atom(Word)].

run(Pid, Data) ->
    Pid!{self(), lex(Data, [])}.

plex(List, Size) ->
    Part = split(List, Size, 1, length(List)),
    W = lists:map(fun(X) ->

                          spawn(?MODULE, run, [self(), X])
                  end, Part),

    lists:map(fun (P) ->
                      receive

{P, V} -> V end

end, W).
```


## 2022-07-06

### Scheme

Consider the technique “closures as objects” as seen in class, where a closure assumes the role of a class. In this technique, the called procedure (which works like a class in OOP) returns a closure which is essentially the dispatcher of the object.

Define the `define-dispatcher` macro for generating the dispatcher in an automatic way, as illustrated by the following example:
```scheme
(define (make-man)
  (let ((p (make-entity))
        name "man"))
  (define prefix+name
    (lambda (prefix)
      (string-append prefix name)))
  (define change-name
    (lambda (new-name)
      (set! name new-name)))
  (define-dispatcher methods: (prefix+name change-name) parent: p)))
```
where `p` is the parent of the current instance of class `man`, and `make-entity` is its constructor.  
If there is no inheritance (or it is a base class), `define-dispatcher` can be used without the parent: `p` part.

Then, an instance of class man can be created and its methods can be called as follows:
```scheme
> (define carlo (make-man))  
> (carlo 'change-name "Carlo")  
> (carlo 'prefix+name "Mr. ")

"Mr. Carlo"
```

Solution:
```scheme
(define (unknown-method ls)
  (error "Unknown method" (car ls)))

(define-syntax define-dispatcher
  (syntax-rules (methods: parent:)

    ((_ methods: (mt ...) parent: p)
     (lambda (message . args)

       (case message
         ((mt) (apply mt args))
         ...
         (else (apply p (cons message args))))))

    ((_ methods: mts)
     (define-dispatcher methods: mts parent: unknown-method))))
```


### Haskell

A *deque*, short for *double-ended queue*, is a list-like data structure that supports efficient element insertion and removal from both its head and its tail. Recall that Haskell lists, however, only support O(1) insertion and removal from their head.

Implement a deque data type in Haskell by using two lists: the first one containing elements from the initial part of the list, and the second one containing elements form the final part of the list, reversed.

In this way, elements can be inserted/removed from the first list when pushing to/popping the deque's head, and from the second list when pushing to/popping the deque's tail.
1) Write a data type declaration for `Deque`. 
2) Implement the following functions:
	- `toList`: takes a `Deque` and converts it to a list
	- `fromList`: takes a list and converts it to a `Deque`
	- `pushFront`: pushes a new element to a `Deque`'s head
	- `popFront`: pops the first element of a `Deque`, returning a tuple with the popped element and the new `Deque`
	- `pushBack`: pushes a new element to the end of a `Deque`
	- `popBack`: pops the last element of a `Deque`, returning a tuple with the popped element and the new `Deque`
3) Make `Deque` an instance of `Eq` and `Show`.  
4) Make `Deque` an instance of `Functor`, `Foldable`, `Applicative` and `Monad`. You may rely on instances of the above classes for plain lists.

### Erlang


## 2022-09-01

### Scheme

We want to implement a version of call/cc, called store-cc, where the continuation is called only once and it is implicit, i.e. we do not need to pass a variable to the construct to store it. Instead, to run the continuation, we can use the associated construct run-cc (which may take parameters). The composition of store-cc must be managed using in the standard last-in-first-out approach.

```scheme
(define *stored-cc* '())
(define-syntax store-cc
  (syntax-rules ()
    ((_ e ...)
     (call/cc (lambda (k)
                (set! *stored-cc* (cons k *stored-cc*))
                e ...)))))
(define (run-cc . v)
  (let ((k (car *stored-cc*)))
    (set! *stored-cc* (cdr *stored-cc*))
    (apply k v)))
```

Here `run-cc` shows how to call a continuation that may read parameters: 
- name (head of the list)`.` list of parameters (tail of the list), similar to Erlang `[H|T]`

### Haskell

We want to implement a binary tree where in each node is stored data, together with the number of nodes contained in the subtree of which the current node is root.
1. Define the data structure.
2. Make it an instance of Functor, Foldable, and Applicative.

--- 
The applicative is the difficult part (aside for pure). It's important to keep in mind that we want apply a bunch of function to a bunch of values and then concatenate the results of each function application together, both function and values are inside a container, which can be a list, a tree, a `Maybe` container (it can hold at max one value/function) and so on. 
Nevertheless, the ingredients for `<*>` (apply) are always the same:
- a `fmap` function, already present from the `Functor`, we need it to apply each function to value(s) in the container. 
- a `concat` function that concatenates a list of containers using `foldr` from `Foldable` and a `+++` operator between two containers
- now we want to extract the value(s) from the container and apply the function to the value(s) and then reinsert the result in the container like this `(a -> Container a) -> Container a -> Container b`,  this is done by`fmap`. The real issue is we want to do this with each functions that is inside the container of the functions. 
- To achieve this we can write the `concatmap` function which applies first `fmap` and then `concat` like this `concatmap f c = concat $ fmap f c`,  the trick here is to apply the value to each function in container.
  That is a job for `fmap` but instead of passing a function and a container of values, we pass a lambda function that takes as argument a function and it applies to the container of values, and we do it for each function like so:
```haskell
x <*> y = concatmap (\f -> fmap f x) y
-- in other terms
x <*> y = foldr concat Empty $ fmap (/f -> fmap f x) y
```

### Erlang

We want to implement a parallel `foldl`, `parfold(F, L, N)`, where the binary operator `F` is associative, and `N` is the number of parallel processes in which to split the evaluation of the fold. Being F associative, `parfold` can evaluate `foldl` on the `N` partitions of `L` in parallel. Notice that there is no starting (or accumulating) value, differently from the standard `foldl`.

You may use the following library functions:
`lists:foldl(<function>, <starting value>, <list>)`
`lists:sublist(<list>, <init>, <length>)`, which returns the sublist of `<list>`starting at position `<init>` and of length `<length>`, where the first position of a list is 1.

The real challenge is how to partition a list:
```erlang
partition(L, N) ->
    M = length(L),
    Chunk = M div N,
    End = M - Chunk*(N-1),
    parthelp(L, N, 1, Chunk, End, []).

parthelp(L, 1, P, _, E, Res) ->
    Res ++ [lists:sublist(L, P, E)];
parthelp(L, N, P, C, E, Res) ->
    R = lists:sublist(L, P, C),
    parthelp(L, N-1, P+C, C, E, Res ++ [R]).
```


## 2023-02-15

### Scheme

```scheme

```

### Haskell

```haskell
data BBTree a = BBNode a a (BBTree a) (BBTree a) | BBEmpty

bb2list BBEmpty = []
bb2list (BBNode a b left right) = [a] ++ [b] ++ bb2list left ++ bb2list right

instance Foldable BBTree where
  foldr _ z BBEmpty = z
  foldr f z (BBNode a b left right) = 
      f a (f b (foldr f (foldr f z right) left))

instance Functor BBTree where
  fmap f BBEmpty = BBEmpty
  fmap f (BBNode a b left right) = BBNode (f a) (f b) (fmap f left) (fmap f right)

instance Applicative BBTree where
  pure x = BBNode x x BBEmpty BBEmpty
  _ <*> BBEmpty = BBEmpty
  BBEmpty <*> _ = BBEmpty
  (BBNode fa fb fleft fright) <*> (BBNode a b left right) = 
    BBNode (fa a) (fb b) (fleft <*> left) (fright <*> right)

bbmax :: Ord a => BBTree a -> Maybe a
bbmax BBEmpty = Nothing
bbmax (BBNode a b left right) = Just (foldr max a (BBNode a b left right))
```

### Erlang

```erlang
```


## 2023-06-12

### Scheme

Write a function, called `fold-left-right`, that computes both `fold-left` and `fold-right`, returning them in a pair. Very important: the implementation must be **one-pass**, for efficiency reasons, i.e. it must consider each element of the input list only once; hence it is not correct to just call Scheme’s `fold-left` and `fold-right`.

Example: `(fold-left-right string-append "" '("a" "b" "c"))` is the pair `("cba" . "abc")`.

```scheme
(define (fold-left-right f z L)
  (let loop ((res (cons z z)) (lf L) (lr (reverse L)))
    (if (null? lf)
        res
        (loop (cons
                    (f (car lf) (car res))
                    (f (cdr res) (car lr))) (cdr lf) (cdr lr)))
    ))
```

### Haskell

Define a **partitioned list** data structure, called `Part`, storing three elements:
1. a **pivot** value,
2. a list of elements that are all less than or equal to the pivot, and
3. a list of all the other elements.

Implement the following utility functions, writing their types:
- `checkpart`, which takes a `Part` and returns true if it is valid, false otherwise;
- `part2list`, which takes a `Part` and returns a list of all the elements in it;
- `list2part`, which takes a pivot value and a list, and returns a `Part`;

Make `Part` an instance of `Foldable` and `Functor`, if possible. If not, explain why.

Solution:
```haskell
data Part a = Part a [a] [a]

checkpart :: Part a -> Bool
checkpart (Part p lesseq greater) 
  | filter (\a -> a <= p) lesseq ++
    filter (\b -> b > p) greater == [] = True
  | otherwise = False

part2list :: Part a -> [a]
part2list (Part a lesseq greater) = lesseq ++ greater

list2part :: a -> [a] -> Part a
list2part p list = 
  Part p (filter (\a -> a <= p) list) (filter (\b -> b > p) list)

instance Foldable Part where 
  foldr f z p = foldr f z $ (part2list p)

-- This will throw an error why?
-- instance Functor Part where
--   fmap f (Part p lesseq greater) = list2part p ((fmap f lesseq) ++ (fmap f greater))
-- https://www.phind.com/search?cache=w28xqm5xgjqc1rk5l46rwnp5

instance Functor Part where
  fmap f (Part p lesseq greater) = 
    let
      newP = f p
      newLesseq = fmap f lesseq
      newGreater = fmap f greater
    in list2part newP (newLesseq ++ newGreater)

(+++) :: Part a -> Part a -> Part a
Part p a b +++ Part q c d = list2part p (a++b++c++d)

instance Applicative Part where
  pure x = Part x [] []
  
  (Part fp flesseq fgreater) <*> (Part xp xlesseq xgreater) =
    let
      -- Apply the function in the primary part to the primary part of the second Part
      newP = fp xp
      
      -- Apply all functions in the lists to the primary part and the lists of the second Part
      -- We concatenate the results to maintain the structure of the Part
      newLesseq = map fp xlesseq ++ concatMap (\f -> map f xlesseq) flesseq
      newGreater = map fp xgreater ++ concatMap (\f -> map f xgreater) fgreater
      
    in Part newP newLesseq newGreater


```

### Erlang

Consider the following implementation of `mergesort` and write a parallel version of it.

```erlang
mergesort([L]) -> [L];
mergesort(L) ->
    {L1, L2} = lists:split(length(L) div 2, L),
    merge(mergesort(L1), mergesort(L2)).

merge(L1, L2) -> merge(L1, L2, []).
merge([], L2, A) -> A ++ L2;
merge(L1, [], A) -> A ++ L1;
merge([H1|T1], [H2|T2], A) when H2 >= H1 -> merge(T1, [H2|T2], A ++ [H1]);
merge([H1|T1], [H2|T2], A) when H1 > H2 -> merge([H1|T1], T2, A ++ [H2]).

```

Solution:
```erlang
```


## 2023-07-03

### Scheme

Define a `let**` construct that behaves like the standard `let*`, but gives to variables provided without a binding the value of the last defined variable. It also contains a default value, stated by a special keyword `def:`, to be used if the first variable is given without binding.

For example:
```scheme
(let** def: #f
  (a (b 1) (c (+ b 1)) d (e (+ d 1)) f)
  (list a b c d e f))
```
should return `'(#f 1 2 2 3 3)`, because `a` assumes the default value `#f`, while `d = c` and `f = e`.

Incorrect solution:
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
%% TODO: add motivation %%

Solution:
```scheme
(define-syntax let**
  (syntax-rules (def:)
    ((_ def: dfvalue (var) body ...)
     (let ((var dfvalue))
       body ...))
    ((_ def: dfvalue ((var value)) body ...)
     (let ((var dfvalue))
       body ...))
    ((_ def: dfvalue ((var value) . rest) body ...)
     (let ((var value))
       (let** def: value rest body ...)))   
    ((_ def: dfvalue (var . rest) body ...)
     (let ((var dfvalue))
       (let** def: dfvalue rest body ...)))))
```
### Haskell

1. Define a data structure, called D2L, to store lists of possibly depth two, e.g. like `[1,2,[3,4],5,[6]]`.
2. Implement a `flatten` function which takes a D2L and returns a flat list containing all the stored values in it in the same order.
3. Make D2L an instance of `Functor`, `Foldable`, `Applicative`.

Solution:
```haskell
data D2L a = D2 a (D2L a) | D1 a (D2L a) | End deriving Show

flatten :: D2L a -> [a]
flatten End = []
flatten (D2 x xs) = [x] ++ flatten xs
flatten (D1 x xs) = [x] ++ flatten xs

instance Functor D2L where
  fmap f End = End
  fmap f (D1 x xs) = D1 (f x) (fmap f xs)
  fmap f (D2 x xs) = D2 (f x) (fmap f xs)

concatd2l :: D2L a -> D2L a -> D2L a
concatd2l End d2l = d2l
concatd2l d2l End = d2l
concatd2l (D1 x xs) d2l = D1 x (concatd2l xs d2l)
concatd2l (D2 x xs) d2l = D2 x (concatd2l xs d2l)

instance Foldable D2L where
  foldr _ z End = z
  foldr f z (D1 x xs) = f x (foldr f z xs)
  foldr f z (D2 x xs) = f x (foldr f z xs)
  
instance Applicative D2L where
  pure x = D1 x End
  End <*> d2l = End
  d2l <*> End = End
  D1 f fs <*> d2l =  fmap f d2l `concatd2l` (fs <*> d2l)
  D2 f fs <*> d2l =  fmap f d2l `concatd2l` (fs <*> d2l)

-- shorter implementation
instance Applicative D2L where
  pure x = D1 x End
  d2f <*> d2l =  foldr concatd2l End (fmap (\ f -> fmap f d2l) d2f)
```

### Erlang

```erlang
```


## 2023-09-12

### Scheme

```scheme
(define-syntax multifun
  (syntax-rules ()
    [(_ (fname ...) (pars) (body ...))
     (begin
       (define (fname pars) body) ...)]))

(define-syntax-rule (multifun (fname ...) (pars) (body ...))
  (begin
    (define (fname pars) body) ...))
```

### Haskell

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)

gentree 0 a = Leaf a
gentree n a = Branch (gentree (n-1) a) (gentree (n-1) a)

-- btrees x = btreesaux 0 x where
--   btreesaux n x = [gentree n x] ++ (btreesaux (n+1) x)

btrees x = map (\n -> gentree n x) [0,1..]
btrees2 x = map (\n -> gentree n (x+n)) [0,1..]

counts :: [Integer]
counts = map (\n -> 2*n + 1) [0..]
```

### Erlang

```erlang

```


## 2024-01-11

### Scheme

```scheme

```

### Haskell

Consider the following type of expressions, containing variables of some type `a`, constants that are integers, and a some kind of binary operator called `Op`.

```haskell
data Expr a = Var a | Const Int | Op (Expr a) (Expr a)
```

1. Make it an instance of Functor, Applicative, and Monad.
2. Using an example, show what the `>>=` operator does in your implementation.

Solution:
```haskell
data Expr a = Var a | Const Int | Op (Expr a) (Expr a) deriving Show

instance Functor Expr where
  fmap f (Var v) = Var (f v)
  fmap _ (Const i) = Const i
  fmap f (Op expra exprb) = Op (fmap f expra) (fmap f exprb)

instance Applicative Expr where
  pure = Var
  Const i <*> _ = Const i
  _ <*> Const i = Const i
  Var f <*> xs = fmap f xs
  (Op fa fb) <*> xs = Op (fa <*> xs) (fb <*> xs)
  
instance Monad Expr where
  Const i >>= f = Const i
  Var v >>= f = f v
  Op expra exprb >>= f = Op (expra >>= f) (exprb >>= f)

expr1 :: Expr String
expr1 = Op (Var "x") (Const 42)

-- Define a function that maps variables to expressions
mapVar :: String -> Expr String
mapVar "x" = Op (Const 1) (Const 2)
mapVar v = Var v

-- Use the >>= operator
result = expr1 >>= mapVar
```

### Erlang

```erlang
```


## 2024-02-02

### Scheme

Consider the following data structure, written in Haskell:

```haskell
data Expr a = Var a | Val Int | Op (Expr a) (Expr a)

instance Functor Expr where
  fmap _ (Val x) = Val x
  fmap g (Var x) = Var (g x)
  fmap g (Op a b) = Op (fmap g a) (fmap g b)

instance Applicative Expr where
  pure = Var
  _ <*> Val x = Val x
  Val x <*> _ = Val x
  Var f <*> Var x = Var (f x)
  Var f <*> Op x y = Op (fmap f x) (fmap f y)
  Op f g <*> x = Op (f <*> x) (g <*> x)

instance Monad Expr where
  Val x >>= _ = Val x
  Var x >>= f = f x
  Op a b >>= f = Op (a >>= f) (b >>= f)
```

Define an analogous in Scheme, with all the previous operations, where the data structures are encoded as lists – e.g. `Op (Val 0) (Var 1)` is represented in Scheme as `'(Op (Val 0) (Var 1))`.

Solution:
```racket

```


### Haskell

```haskell

```

### Erlang

```erlang
```

