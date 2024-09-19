## Functions

### Factorial Function

Write a function `fact` that takes an integer and returns the factorial of that integer.

Example:
```haskell
fact 5 -- Output: 120
```

Solution:
```haskell
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
```

>[!note]- `Int` vs `Integer`
>`Int` is a fixed-size integer type. It can represent integers within a finite range. The language standard guarantees a range of -2^29 to (2^29 - 1). Most implementations will provide a full machine-size signed integer, i.e., 32 or 64 bits. Operations on `Int` can be much faster than operations on `Integer`, but overflow and underflow can cause unexpected bugs. Using `Int` in an initial design could be considered premature optimization. However, many standard library functions (e.g., `length`, `take`) use `Int` [1](https://wiki.haskell.org/FAQ).
> 
> On the other hand, `Integer` is an arbitrary precision integer type. It can represent arbitrarily large integers, up to using all of the storage on your machine. This means you never have arithmetic overflows. However, it also means your arithmetic is relatively slower compared to `Int` [8](https://stackoverflow.com/questions/3429291/what-is-the-difference-between-int-and-integer).

### List Length Function

Write a function `len` that takes a list and returns the length of the list.

Example:
```haskell
len [1,2,3,4,5] -- Output: 5
```

Solution:
```haskell
len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs
```

Alternative solution using `foldr`:
```haskell
len :: [a] -> Int
len = foldr (\_ acc -> acc + 1) 0
```

### Reverse List Function

Write a function `rev` that takes a list and returns the list in reverse order.

Example:
```haskell
rev [1,2,3,4,5] -- Output: [5,4,3,2,1]
```

Solution:
```haskell
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]
```

Alternative solution using `foldl`:
```haskell
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []
```
### FoldLeft Function

Write a function `foldleft` that takes a binary function, an initial value, and a list, and applies the function to the list in a left-associative manner.

Example:
```haskell
foldleft (+) 0 [1,2,3,4,5] -- Output: 15
```

Solution:
```haskell
foldleft :: (a -> b -> b) -> b -> [a] -> b
foldleft f z [] = z
foldleft f z (x:xs) = foldleft f (f x z) xs
```


### Reverse List Function using FoldLeft

Write a function `rev'` that takes a list and returns the list in reverse order, using `foldleft`.

Example:
```haskell
rev' [1,2,3,4,5] -- Output: [5,4,3,2,1]
```

The solution presented is wrong, explain why.
```haskell
rev' :: [a] -> [a]
rev' = foldl (:) []
```

The cons operator `:` has as first argument an element of type `a` and as second a list of type `a`:
```haskell
(:) :: a -> [a] -> [a]

foldl (:) [] [1,2,3,4,5]
foldl (:) ([] : 1) [2,3,4,5] -- error
```

Correct approach:
```haskell
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []
```

## Data Types

### Traffic Light Data Type

Define a data type `TrafficLight` with three constructors: `Red`, `Yellow`, and `Green`. Implement `Show` and `Eq` instances for `TrafficLight`.

Example:
```haskell
show Red -- Output: "red"
Red == Red -- Output: True
```

Solution:
```haskell
data TrafficLight = Red | Yellow | Green

instance Show TrafficLight where
   show Red = "red"
   show Yellow = "yellow"
   show Green = "green"

instance Eq TrafficLight where
   Red == Red = True
   Yellow == Yellow = True
   Green == Green = True
   _ == _ = False
```

>[!note] `putStrLn` vs `print`
> ```haskell
> module Main where
> 
> data TrafficLight = Red | Yellow | Green deriving (Show,Eq)
> 
> main = do
>   putStrLn (show Red)
>   print Red
> ```
>The `putStrLn` function is used to write a string to the terminal, followed by a newline character. The `show` function is used to convert the `Red` value to a string before it's passed to `putStrLn`. So, this line is converting `Red` to a string, and then printing that string to the terminal, followed by a newline [2](https://hoogle.haskell.org/?q=putStrLn), [6](https://stackoverflow.com/questions/19288652/difference-between-print-and-putstrln-in-haskell).
>The `print` function in Haskell is used to print a value to the terminal. However, unlike `putStrLn`, `print` can work with any value, not just strings. This is because `print` uses the `Show` typeclass to convert its argument to a string before printing it. The `Show` typeclass provides a way to convert values to strings in Haskell. So, this line is converting `Red` to a string using the `Show` typeclass, and then printing that string to the terminal [6](https://stackoverflow.com/questions/19288652/difference-between-print-and-putstrln-in-haskell).


### Point Distance

First define the `Point` data type, then write a function that calculates the `distance` between two points.

Example:
  ```haskell
  p1 = Point 3.0 4.0
  p2 = Point 6.0 8.0
  distance p1 p2
  -- The output should be 5.0
  ```

Solution:
```haskell
pointx (Point x _) = x
pointy (Point _ y) = y

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = 
    let dx = x1 - x2
        dy = y1 - y2
    in sqrt $ dx^2 + dy^2
```

>[!note]-
> 
> The `pointx` and `pointy` functions extract the x and y coordinates from a `Point` value respectively. This is achieved using pattern matching where the underscore `_` is used to ignore the part of the data we're not interested in.
> ```haskell
> pointx (Point x _) = x
> pointy (Point _ y) = y
> ```
> 
> The `APoint` data type is another representation of a point in 2D space, but uses the record syntax `{}`. This automatically generates functions `apx` and `apy` that extract the x and y coordinates from an `APoint` value respectively.
> ```haskell
> data APoint = Apoint { apx, apy :: Float } deriving (Show, Eq)
> ```
> 
> The `TPoint` type is a type synonym for a pair of `Float` values. It's another way to represent a point in 2D space. The `distancet` function calculates the Euclidean distance between two `TPoint` values in a similar way to the `distance` function.
> ```haskell
> type TPoint = (Float, Float)
> 
> distancet :: TPoint -> TPoint -> Float
> distancet (x1,y1) (x2,y2) = 
>     let dx = x1 - x2
>         dy = y1 - y2
>     in sqrt $ dx^2 + dy^2 
> 
> ```
> 
> The `pointxt` and `pointyt` functions extract the x and y coordinates from a `TPoint` value respectively. They use the `fst` and `snd` functions which return the first and second elements of a pair.
> ```haskell
> pointxt = fst
> pointyt = snd
> ```
> 


## Lists
### List Filter

Implement a filtering function `myFilter :: (a -> Bool) -> [a] -> [a]` that takes a predicate function and a list, and returns a new list with elements that satisfy the predicate.

Example:
  ```haskell
myFilter even [1,2,3,4,5,6] -- The output should be [2,4,6]
  ```

Solution:
```haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (h:t)
  | f h = h : myFilter f t
  | otherwise = myFilter f t
```

### Custom Zip

Write a custom function called `myZip` that mimics the behavior of the standard `zip` function. The function should take two lists as input and return a list of tuples. If the lists are of unequal length, the resulting list should be as long as the shorter list.

**Example:** 
```haskell
myZip [1, 2, 3] [4, 5, 6]
-- Output: [(1,4),(2,5),(3,6)]
```

**Solution:**
```haskell
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
```

The function uses pattern matching to check if either of the input lists is empty. If either list is empty, it returns an empty list. If neither list is empty, it constructs a tuple from the heads of the two lists, then recurses on the tails of the two lists. The constructed tuple is prepended to the result of the recursive call, building up the list of tuples. 


### Custom ZipWith

Write a function in Haskell called `myZipWith`. The `myZipWith` function should take three arguments: a binary function `f`, and two lists. The function should combine the two lists into a single list by applying the function `f` to the corresponding elements of the two lists. If the lists are of different lengths, the function should stop processing once it has reached the end of the shortest list. 

**Example:**
```haskell
myZipWith (+) [1, 2, 3] [4, 5, 6]
-- This will output: [5, 7, 9]
```

**Solution:**
```haskell
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys
```


### List Sum

Write a function `sumf` that takes a list of numbers and returns the sum of all elements in the list using `foldr`.

Example:
```haskell
sumf [1,2,3,4,5] -- Output: 15
```

Solution:
```haskell
sumf :: Num a => [a] -> a
sumf = foldr (+) 0
```

>[!help] Explanation
`Num a => [a] -> a` is the function's type signature. It means that the function takes a list of numeric values (`[a]`) and returns a numeric value (`a`). The `Num a =>` part is a type constraint, specifying that `a` must belong to the `Num` type class, which includes types that can be used in numeric operations.
`foldr (+) 0` is the function definition. It uses the `foldr` function from Haskell's Prelude library. `foldr` is a higher-order function that takes a binary function, a starting value (also known as an accumulator), and a list, then applies the function to the list elements and the accumulator from right to left.

>[!note]- `foldr`
> The type signature of `foldr` is `(a -> b -> b) -> b -> [a] -> b`. It takes three arguments: a binary function, an initial accumulator value, and a list.
> 
> The binary function takes two inputs: the first is an element from the list, and the second is the accumulator. The function is applied to each element of the list in turn, along with the current value of the accumulator. The result of each function application becomes the new accumulator for the next step. The process continues until all elements in the list have been processed.
> 
> The initial accumulator value is used as the starting point for the folding process. This value is used as the second input to the binary function during the first step of the fold.
> 
> Here's a simple example of how `foldr` works:
> ```haskell
> foldr (+) 0 [1, 2, 3]
> ```
> This will result in the computation `1 + (2 + (3 + 0))`, which equals `6`.
> 
> One important characteristic of `foldr` is that it can operate on infinite lists and produce meaningful results, thanks to Haskell's lazy evaluation. This is because `foldr` starts processing the list from the right, and if the binary function can produce part of its result without reference to the recursive case, the recursion can stop early.
> 
> Here's an example where `foldr` operates on an infinite list:
> ```haskell
> take 5 $ foldr (\i acc -> i : fmap (+3) acc) [] (repeat 1)
> ```
> This will result in the list `[1,4,7,10,13]`.


###  Element in List

Write a function `elemf` that takes an element and a list, and returns `True` if the element is in the list and `False` otherwise, using `foldr`.

Example:
```haskell
elemf 3 [1,2,3,4,5] -- Output: True
elemf 7 [1,2,3,4,5] -- Output: False
```

Solution:
```haskell
elemf :: Eq a => a -> [a] -> Bool
elemf x = foldr (\a b -> a == x || b) False
```

>[!help]- Explanation
>For `elemf`, the binary function is `(\a b -> a == x || b)`, the starting value is `False`, and the list is the input to `elemf`. The binary function checks if the current element `a` is equal to `x` or if the previous result `b` is `True`. If either is `True`, it returns `True`; if not, it moves on to the next element. If no elements are equal to `x`, it returns the starting value, `False` [Source 0](https://stackoverflow.com/questions/65060703/haskell-own-version-of-elem-function-using-foldl).
> 
> In simpler terms, this function checks if a given element is present in a list. It returns `True` if the element is found, and `False` otherwise.
> 
> Here is how it works on a sample list:
> ```haskell
> elemf 2 [1, 2, 3, 4, 5]
> ```
> 
> The `foldr (\a b -> a == x || b) False` takes the list `[1, 2, 3, 4, 5]` and applies the function `(\a b -> a == x || b)` to each element of the list and the current result, starting with `False`. The computation proceeds as follows:
> ```haskell
> 1 == 2 || (2 == 2 || (3 == 2 || (4 == 2 || (5 == 2 || False))))
> ```
> This eventually evaluates to `True`, because `2` is present in the list.

>[!note] Lamba function
>A lambda expression in Haskell starts with a backslash (`\`), followed by one or more arguments, an arrow (`->`), and then the body of the function.
> 
> Here is a simple example of a lambda function:
> ```haskell
> \x -> x + 1
> ```
> This function takes one argument `x` and adds 1 to it. You can apply this function to a value like this:
> ```haskell
> (\x -> x + 1) 4 -- returns 5
> ```
> 
> Lambda functions can also take multiple arguments:
> ```haskell
> \x y -> x + y
> ```
> This function takes two arguments `x` and `y`, and returns their sum. You can apply this function to two values like this:
> ```haskell
> (\x y -> x + y) 3 5 -- return 8
> ```


### Map Function

Write a function `mapf` that takes a function and a list, and returns a new list made by applying the function to each element of the original list, using `foldr`.

Example:
```haskell
mapf (*2) [1,2,3,4,5] -- Output: [2,4,6,8,10]
```

Solution:
```haskell
mapf :: (a -> b) -> [a] -> [b]
mapf f = foldr (\a b -> f a : b) []
```


## Binary Trees

[Solutions](https://replit.com/@glazzarelli/Haskell-Binary-Tree-demo) to these exercises.
### Binary Tree Data Type

Define a binary tree data structure `BTree` for the binary tree data structure and write and instance of the `Show` class for `Btree`.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
show t1 --  [ 1 [ 2 ]  [ 3 [ 4 ]  [ 5 ]  ]  ] 
```

Solution:
```haskell
data BTree a = BEmpty | BNode a (BTree a) (BTree a)
bleaf a = BNode a BEmpty BEmpty

instance Show a => Show (BTree a) where  
  show BEmpty = ""
  show (BNode v left right) = " [ " ++ show v ++ show left ++ show right ++ " ] "
```

>[!note] In `bleaf a`, `a` is not a generic type
>`bleaf` is a function that takes a value `a` (not type a) and constructs a new tree node with that value and two empty child trees. It is not a method that is called on an object, but rather a standalone function. You can use this function to create a new tree node with a specific value.

### Binary Tree Map Function

Implement a function `bmap :: (a -> b) -> BTree a -> BTree b` that applies a function to every element in the binary tree.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
bmap (+1) tree --  [ 2 [ 3 ]  [ 4 [ 5 ]  [ 6 ]  ]  ] 
```

Solution:
```haskell
bmap :: (a -> b) -> BTree a -> BTree b
bmap _ BEmpty = BEmpty
bmap f (BNode v left right) = BNode (f v) (bmap f left) (bmap f right)
```


### Binary Tree to List

Write a function `bToList` that takes a binary tree and returns a list of its elements.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
bToList t1 -- Output: [1,2,3,4,5]
```

Solution:
```haskell
bToList :: (BTree a) -> [a]
bToList BEmpty = []
bToList (BNode v x y) = [v] ++ (bToList x) ++ (bToList y)
```


### Infinite Binary Trees

Create an infinite binary tree with `binf`, then implement a function `btake` to take the first `n` levels of the tree.

Example:
```haskell
btake 2 (binf 1)
-- Expected output: "[ 4 [ 5 [ 6 ]  [ 6 ]  ]  [ 5 [ 6 ]  [ 6 ]  ]  ]"
```

Solution:
```haskell
binf :: Integer -> BTree Integer
binf n = BNode n (binf (n+1)) (binf (n+1))

btake :: Integer -> BTree a -> BTree a
btake _ BEmpty = BEmpty
btake n (BNode v left right) 
  | n > 0 = BNode v (btake (n-1) left) (btake (n-1) right) 
  | otherwise = BEmpty
```

Alternative solution:
```haskell
binf :: Integer -> BTree Integer
binf x = let t = binf (x+1) 
         in BNode x t t

btake _ BEmpty = BEmpty
btake 0 _ = BEmpty
btake n (BNode v l r) = BNode v (btake (n-1) l) (btake (n-1) r)
```

### Equality of Binary Trees

Define an instance of the `Eq` type class for the `BTree` data type. Two binary trees are considered equal if they contain the same elements.

Example:
```haskell
t1 == t2 -- Output: True or False
```

Solution:
```haskell
instance Eq a => Eq (BTree a) where
    x == y = (bToList x) == (bToList y)
```

Alternative Solution:
```haskell
instance Eq a => Eq (BTree a) where
  BEmpty == BEmpty = True
  BNode v1 l1 r1 == BNode v2 l2 r2 = v1 == v2 && l1 == l2 && r1 == r2 
  _ == _ = False
```


### Binary Tree Fold Function

Write a function `btfold` that takes a binary function, a binary tree, and an initial value, and applies the binary function to the elements of the binary tree and the initial value.

Example:
```haskell
t4 = BNode 7 (BNode 3 (bleaf 4) (bleaf 9)) (bleaf 11)
btfold (+) t 0 -- Output: 34
```

Solution:
```haskell
btfold :: (a -> b -> b) -> BTree a -> b -> b
btfold _ BEmpty acc = acc
btfold f (BNode v left right) i = f v (btfold f left (btfold f right i))
```

### Binary Tree Filter Function

Write a function `btfilter` that takes a predicate and a binary tree, and returns a new binary tree containing only the elements of the original tree that satisfy the predicate.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
btfilter (>3) t1 -- Output: <4 5>
```

Solution:
```haskell
btfilter :: (a -> Bool) -> BTree a -> BTree a
btfilter _ BEmpty = BEmpty
btfilter pred (BNode x left right)
    | pred x    = BNode x (btfilter pred left) (btfilter pred right)
    | otherwise = merge (btfilter pred left) (btfilter pred right)
  where
    -- Merge two binary trees
    merge :: BTree a -> BTree a -> BTree a
    merge BEmpty t = t
    merge t BEmpty = t
    merge t1 (BNode x left right) = BNode x (merge t1 left) right
```

### Binary Tree Concat Function

Write a function `btconcat` that takes two binary trees and returns a binary tree containing the elements of the binary trees.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
t2 = BNode 6 (bleaf 7) (bleaf 8)

btconcat t1 t2
-- Output:  [ 6 [ 7 [ 1 [ 2 ]  [ 3 [ 4 ]  [ 5 ]  ]  ]  ]  [ 8 ]  ] 
```

Solution:
```haskell
btconcat :: BTree a -> BTree a -> BTree a
btconcat BEmpty t = t
btconcat t BEmpty = t
btconcat t1 (BNode x left right) = BNode x (btconcat t1 left) right
```

### Binary Tree ConcatMap Function

Write a function `btconcatmap` that takes a function and a list of binary trees, and returns a tree concatenation of the results of applying the function to the elements of each binary trees.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
t2 = BNode 6 (bleaf 7) (bleaf 8)
btconcatmap (*2) [t1,t2] -- Output: [2[4][6[8][10[12[14][16]]]]]
```

Solution:
```haskell
btconcatmap :: (a -> b) -> [BTree a] -> BTree b
btconcatmap f [] = BEmpty
btconcatmap f treelist = bmap f (foldr bconcat BEmpty treelist)
  where
    bconcat BEmpty tree = tree
    bconcat tree BEmpty = tree
    bconcat (BNode el left right) tree = BNode el left (bconcat right tree)
```


### Binary Tree Instance of Functor

Write an instance of the Functor class for the binary tree data type.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
fmap (*2) t1 -- Output: [ 4 2 8 4 10 ]
```

Solution:
```haskell
instance Functor BTree where
   fmap = btmap
```

### Binary Tree Instance of Foldable

Write an instance of the Foldable class for the binary tree data type.

Example:
```haskell
t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
foldr (+) 0 t1 -- Output: 15
```

Solution:
```haskell
btfold :: (a -> b -> b) -> BTree a -> b -> b
btfold _ BEmpty acc = acc
btfold f (BNode v left right) i = f v (btfold f left (btfold f right i))

instance Foldable BTree where
   foldr f acc tree = btfold f tree acc
```

### Binary Tree Instance of Applicative

Write an instance of the Applicative class for the binary tree data type.

Example:
```haskell
btFuncs = BNode (+1) (bleaf (*2)) (bleaf (`div` 2))
btValues = BNode 1 (bleaf 2) (bleaf 3)

result = btFuncs <*> btValues
-- [ 0 [ 1 [ 2 [ 4 [ 2 [ 3 ]  [ 4 ]  ]  ]  [ 6 ]  ]  ]  [ 1 ]  ]
```

A possible solution:
```haskell
instance Applicative BTree where
  pure = bleaf
  ftree <*> tree = foldr btconcat BEmpty (fmap (\ f -> fmap f tree) ftree)
```

Since the `ftree` contains the functions to apply and has more than one node (function), we can apply each function to `tree` and then concatenate the results.

Visual representation of the computation:
```
First each function is applied to the tree:
  2       2       0           
 / \     / \     / \   => then the results are concatenated:
3   3   4   6   1   1

					  2
					/   \
				   3     4
						  \   
	         	           0
						 /   \
						1     1
							   \
							    2
						      /   \
							 4     6
```


## ZipList Object

[Solutions](https://replit.com/@glazzarelli/ZipList-Object#Main.hs) to these exercises.

### Implementing a ZipList

Given the data type `ZL` for a ZipList, implement the `Show` instance for `ZL`.

**Example:**
```haskell
l1 = ZCons 1 (ZCons 2 (ZCons 3 ZEmpty))
show l1 -- Output: "{1,2,3}"
```

**Solution:**
```haskell
data ZL a = ZEmpty | ZCons a (ZL a) deriving Eq

instance Show a => Show (ZL a) where
   show ZEmpty = "{}"
   show (ZCons x ZEmpty) = "{" ++ show x ++ "}"
   show (ZCons x xs) = "{" ++ show x ++ "," ++ (drop 1 (show xs))
```
The `drop 1` function in Haskell is used to remove the first element from a list or a string.

Alternative solution without `drop`:
```haskell
instance Show a => Show (ZL a) where
  show ZEmpty = "{}"
  show ziplist = "{" ++ show' ziplist ++ "}"
    where 
          show' ZEmpty = ""
          show' (ZCons x ZEmpty) = show x 
          show' (ZCons x xs) = show x ++ "," ++ show' xs
```

### Converting a List to a ZipList

Implement a function `toZipList` that converts a list to a ZipList.

**Example:**
```haskell
l2 = toZipList [1,3..5]
show l2 -- Output: "{1,3,5}"
```

**Solution:**
```haskell
toZipList :: [a] -> ZL a
toZipList [] = ZEmpty
toZipList (x:xs) = ZCons x (toZipList xs)
```

Alternative solution:
```haskell
toZipList :: [a] -> ZL a
toZipList = foldr ZCons ZEmpty 
```


### Functor for ZipList

Implement the `Functor` instance for `ZL`.

**Example:**
```haskell
l3 = fmap (*2) l1
show l3 -- Output: "{2,4,6}"
```

**Solution:**
```haskell
instance Functor ZL where
   fmap f ZEmpty = ZEmpty
   fmap f (ZCons x xs) = ZCons (f x) (fmap f xs)
```

### Foldable for ZipList

Implement the `Foldable` instance for `ZL`.

**Example:**
```haskell
sum l1 -- Output: 6
```

**Solution:**
```haskell
instance Foldable ZL where
   foldr f z ZEmpty = z
   foldr f z (ZCons x xs) = f x (foldr f z xs)
```


### Applicative for ZipList

Implement the `Applicative` instance for `ZL`.

**Example:**
```haskell
l4 = ZCons (* 2) (pure (+ 1)) <*> l1
show l4 -- Output: "{2,4,6,2,3,4}"
```

**Solution:**
```haskell
zlconcat ZEmpty z = z
zlconcat z ZEmpty = z
zlconcat (ZCons x xs) l = ZCons x (zlconcat xs l)

instance Applicative ZL where
  pure x = ZCons x ZEmpty
  ZEmpty <*> _ = ZEmpty
  zlfun <*> zlval = foldr zlconcat ZEmpty $ fmap (`fmap` zlval) zlfun

-- same as:
-- zlfun <*> zlval = foldr zlconcat ZEmpty (fmap (\f -> fmap f zlval) zlfun)
```


## Logging Operations 

[Solutions](https://replit.com/@glazzarelli/Logger-Object#Main.hs) to these exercises.

### Logger Object

The `Logger` object enables logging (strings) alongside computations.
It can be defined as follows:
```haskell
type Log = [String]
data Logger a = Logger a Log

instance (Eq a) => Eq (Logger a) where
    (Logger x _) == (Logger y _) = x == y

instance (Show a) => Show (Logger a) where
    show (Logger d l) = show d 
        ++ "\nLog:" ++
        foldr (\line acc-> "\n\t" ++ line ++ acc) "" l

instance Functor Logger where
    fmap f (Logger x l) = Logger (f x) l
```

### Logger instance of Applicative

Implement the `Applicative` instance of `Logger`.

Example:
```haskell
logger1 = Logger (+1) ["Log1: +1"]
logger2 = Logger 7 ["Log2: 7"]
logger3 = Logger (+2) ["Log3: +2"]

logger3 <*> (logger1 <*> logger2) -- output:
-- 10
-- Log:
--     Log3: +2
--     Log1: +1
--     Log2: 7
```

Solution:
```haskell
instance Applicative Logger where
  pure x = Logger x []
  (Logger fun xs) <*> (Logger val ys) = Logger (fun val) (xs ++ ys)
```


### Logger instance of Monad

Implement the `Monad` instance of `Logger`.

Example:
```haskell
main :: IO ()
main = do
  let logger1 = Logger 3 ["Initialized logger1 with 3"]
      logger2 = Logger 4 ["Initialized logger2 with 4"]
      addLog x y = Logger (x + y) ["Added " ++ show x ++ " and " ++ show y]
  print $ logger1 >>= (\x -> addLog x 5) >>= (\x -> addLog x 10)

-- 18
-- Log:
--     Initialized logger1 with 3
--     Added 3 and 5
--     Added 8 and 10
```

Solution:
```haskell
instance Monad Logger where
    (Logger x l) >>= f = 
        let Logger x' l' = f x
        in  Logger x' (l ++ l')
```

>[!help] Explanation
>The `>>=` operator takes a `Logger` value and a function that returns a `Logger` value. It applies the function to the data part of the `Logger`, and concatenates the logs of the original `Logger` and the new `Logger` obtained from the function: 
>- `(Logger x l) >>= f =`: This line defines the bind operation `(>>=)` for the `Logger` monad. The operation takes a `Logger` value and a function `f` that returns a `Logger` value. The `Logger` value is represented as `(Logger x l)`, where `x` is the data part and `l` is the log messages.
> - `let Logger x' l' = f x`: This line applies the function `f` to the data part `x` of the `Logger` value, and binds the result to `(Logger x' l')`. Here, `x'` is the data part and `l'` is the log messages of the new `Logger` value obtained from the function `f`.
> - `in Logger x' (l ++ l')`: This line constructs a new `Logger` value with the data part `x'` and the log messages `l ++ l'`. The log messages `l` of the original `Logger` and the log messages `l'` of the new `Logger` obtained from the function `f` are concatenated using the list concatenation operator `(++)`.
> This implementation of `(>>=)` for the `Logger` monad allows chaining computations while accumulating log messages. When a `Logger` value is bound to a function that returns a `Logger` value, the function is applied to the data part of the `Logger`, and the logs of the original `Logger` and the new `Logger` obtained from the function are concatenated. This is how the `Logger` monad keeps track of log messages along with computations.


### Logging Leaf Creation

Given the `BTree` data object, create the `bleafM` function that creates a leaf and logs the operation.

```haskell
data BTree a = BEmpty | BNode a (BTree a) (BTree a) deriving Eq
bleaf x = BNode x BEmpty BEmpty

instance Show a => Show (BTree a) where
    show BEmpty = ""
    show (BNode v x y) = "<" ++ show x ++ " " ++ show v ++ " " ++ show y ++ ">" 

t1 = BNode 1 (bleaf 2) (BNode 3 (bleaf 4) (bleaf 5))
t2 = BNode 2 (BNode 2 (bleaf 3) (bleaf 2)) (bleaf 5)
```

Example:
```haskell
bleafM 5 -- output:
-- < 5 >
-- Log:
--     Created leaf 5
```

A solution that does not leverage the bind operation:
```haskell
bleafM :: Show a => a -> Logger (BTree a)
bleafM el = Logger (bleaf el) ["Created leaf " ++ show el]
```

Alternative solution using the bind `>>=` :
```haskell
putLog :: String -> Logger ()
putLog s = Logger () [s] 

bleafM x =
  putLog ("Created leaf " ++ show x) >>= \_ ->
  return (bleaf x)
```

The previous solution using the `do` notation:
```haskell
putLog :: String -> Logger ()
putLog s = Logger () [s] 

bleafM x = do
  putLog $ "Created leaf " ++ show x
  return $ bleaf x
```

> [!help] Explanation
> 
> Changing from `do` notation to `>>=`, this is equivalent to:
> ```haskell
> bleafM :: Show a => a -> Logger (Tree a)
> bleafM x =
>   putLog ("Created leaf " ++ show x) >>= \_ ->
>   return (bleaf x)
> ```
> 1. **First Monadic Action**: `putLog ("Created leaf " ++ show x)` is the first monadic action that logs the message "Created leaf " followed by the string representation of `x`. This returns `Logger () [String]`.
> 2. **Binding with >>=**: The bind operator `>>=` is used to chain the result of the first monadic action to the next. Since `putLog` returns `Logger () [String]`, the bind operator receives `Logger` and ignores the unit value `()` with `\_ ->`.
> 3. **Second Monadic Action**: `return (bleaf x)` is the second monadic action that returns the `Logger (Tree a)` containing the newly created leaf. The default implementation of `return` in a `Monad` can be derived from the `Applicative`'s `pure`.
> 
> Actually, the default definition of `>>` in the `Monad` class is given by:
> ```haskell
> (>>) :: Monad m => m a -> m b -> m b
> a >> b = a >>= \_ -> b
> ```
> 
> In this case we can use the `>>` ("then") operator that discards the unit value `()`:
> ```haskell
> bleafM x =
>   putLog ("Created leaf " ++ show x) >>
>   return (bleaf x)
> ```
> 
> When we use the `>>` operator in the context of the `Logger` monad, we're discarding the value of the first monadic action but not its side effects. In the case of the `Logger` monad, the side effect is appending the log message.

>[!note]- Haskell Unit Type `()`
> 
> The type `()`, often pronounced "Unit", is a type containing one value worth speaking of: that value is also written `()` but in the expression language, and is sometimes pronounced "void". A type with only one value is not very interesting. A value of type `()` contributes zero bits of information: you already know what it must be. So, while there is nothing special about type `()` to indicate side effects, it often shows up as the value component in a monadic type. Monadic operations tend to have types which look like:
> 
> ```haskell
> val-in-type-1 -> ... -> val-in-type-n -> effect-monad val-out-type
> ```
> 
> where the return type is a type application: the (type) function tells you which _effects_ are possible and the (type) argument tells you what sort of _value_ is produced by the operation. For example
> 
> ```haskell
> put :: s -> State s ()
> ```
> 
> which is read (because application associates to the left) as:
> 
> ```haskell
> put :: s -> (State s) ()
> ```
> 
> has one value input type `s`, the effect-monad `State s`, and the value output type `()`. When you see `()` as a value output type, that just means "this operation is used only for its _effect_; the value delivered is uninteresting". Similarly
> 
> ```haskell
> putStr :: String -> IO ()
> ```
> 
> delivers a string to `stdout` but does not return anything exciting.
> 
> The `()` type is also useful as an element type for _container_-like structures, where it indicates that the data consists just of a _shape_, with no interesting payload. For example, if `Tree` is declared as above, then `Tree ()` is the type of binary tree shapes, storing nothing of interest at nodes. Similarly `[()]` is the type of lists of dull elements, and if there is nothing of interest in a list's elements, then the only information it contributes is its length. [Source](https://stackoverflow.com/a/16893900)

### Tree Replacement with Logging

Implement a function `treeReplaceM` that replaces a value in a binary tree with another value and logs the replacement.

Example:
```haskell
let tree = BNode 1 (bleaf 1) (BNode 1 (bleaf 1) (bleaf 1))
print $ treeReplaceM tree 1 2
-- << 2 > 2 << 2 > 2 < 2 >>>
-- Log:
--     replaced 1 with 2
--     replaced 1 with 2
--     replaced 1 with 2
--     replaced 1 with 2
--     replaced 1 with 2
```

Solution:
```haskell
-- Define the tree replacement function
treeReplaceM :: (Eq a, Show a) => BTree a -> a -> a -> Logger (BTree a)
treeReplaceM BEmpty _ _ = return BEmpty
treeReplaceM (BNode v l r) x y = do
    newl <- treeReplaceM l x y
    newr <- treeReplaceM r x y
    if v == x then do 
        putLog $ "replaced " ++ show x ++ " with " ++ show y
        return $ BNode y newl newr 
    else
        return $ BNode v newl newr
```

>[!help] Explanation
> The `<-` operator in Haskell's `do` notation is used to extract the result from a monadic action and bind it to a variable. This allows you to work with the value inside the monad within the `do` block. 
> 
> The snippet above can be translate in terms of `>>=`:
> ```haskell
> treeReplaceM :: (Eq a, Show a) => BTree a -> a -> a -> Logger (BTree a)
> treeReplaceM BEmpty _ _ = return BEmpty
> treeReplaceM (BNode v l r) x y =
>     treeReplaceM l x y >>= \newl ->
>     treeReplaceM r x y >>= \newr ->
>     if v == x then
>         putLog ("replaced " ++ show x ++ " with " ++ show y) >>= \_ ->
>         return (BNode y newl newr)
>     else
>         return (BNode v newl newr)
> ```

### Binary Tree Construction

Implement the `buildTreeM` function that constructs a binary tree where each node's value is half of its parent node's value. The creation of each node should be logged.

Example:
```haskell
print $ buildTreeM 3
-- <<< 0 > 1 < 0 >> 3 << 0 > 1 < 0 >>>
-- Log:
--     Added node 3
--     Added node 1
--     Created leaf 0
--     Created leaf 0
--     Added node 1
--     Created leaf 0
--     Created leaf 0
```

Solution:
```haskell
buildTreeM :: Int -> Logger (BTree Int)
buildTreeM 0 = bleafM 0
buildTreeM x = do
    putLog $ "Added node " ++ show x
    l <- buildTreeM (x `div` 2)
    r <- buildTreeM (x `div` 2)
    return $ BNode x l r
```

>[!help] Explanation
>This function can be translated to use the `>>=` operator and `pure` instead of `return` as follows:
> ```haskell
> buildTreeM x = 
>     putLog ("Added node " ++ show x) >>= 
>         \_ -> buildTreeM (x `div` 2) >>= 
>             \l -> buildTreeM (x `div` 2) >>= 
>                 \r -> pure (BNode x l r)
> ```
> 
> Now, let's break down this code:
> 
> 1. `putLog $ "Added node " ++ show x` is an I/O action that logs a message. The `$` operator is used for function application, it's essentially replacing parentheses. So, `putLog $ "Added node " ++ show x` is equivalent to `putLog ("Added node " ++ show x)`. The `putLog` function takes a String as an argument and returns an I/O action.
> 
> 2. `>>=` is the bind operator. It takes a monadic value (in this case, the I/O action from `putLog`) and a function that takes a normal value and returns a monadic value. It feeds the result of the I/O action into the function. In this case, the function is a lambda function `\_ -> ...` that ignores its input (since `putLog` doesn't produce a meaningful result value) and creates further monadic computations.
> 
> 3. `buildTreeM (x` div `2)` is a recursive call to the function itself, with `x` divided by 2. This is also a monadic action (it builds a part of the tree and possibly logs more messages). It's used twice, once for `l` and once for `r`, creating the left and right subtrees.
> 
> 4. The final part, `\r -> pure (BNode x l r)`, uses the `pure` function to lift a normal value into the monad. It constructs a `BNode` with the original `x` and the subtrees `l` and `r` built by the recursive calls. The `pure` function is essentially the same as `return` in this context, but it's more general because it works in any Applicative functor, not just in Monads.


## LolStream Data Type

[Solutions](https://replit.com/@glazzarelli/lolStream-Data-Type#Main.hs) to these exercises.
### Define LolStream Data Type

Create a data type `LolStream a` that consists of an integer and a list of elements of type `a`. Then define a function `isPeriodic` that takes a `LolStream a` as an argument and checks if the stream is periodic. A stream is periodic if the integer parameter of the `LolStream` is greater than 0. The parameter `n` is the length of the periodic part of the stream.

Example:
```haskell
let stream = LolStream 5 [1, 2, 3, 4, 5]
print $ isPeriodic stream
-- True
```

Solution:
```haskell
data LolStream a = LolStream Int [a]

isPeriodic :: LolStream a -> Bool
isPeriodic (LolStream n _) = n > 0
```

### Define destream Function

Implement the `destream` function. This function should take a `LolStream` and return a list of elements. If the `LolStream` is not periodic, it should return the entire list. Otherwise, it should return the first `n` elements of the list.

Example:
```haskell
let stream = LolStream 5 $ cycle [1, 2, 3, 4, 5]
print $ destream $ stream
```

Solution:
```haskell
destream :: LolStream a -> [a]
destream (LolStream n l) = if n <= 0 then l else take n l
```

### Show Instance for LolStream

Implement the `Show` instance for the `LolStream` data type. The `Show` instance should represent the `LolStream` in a readable format.

```haskell
let stream = LolStream 5 [1, 2, 3, 4, 5]
print stream 
-- LolStream[1,2,3,4,5]
```

Solution:
```haskell
instance (Show a) => Show (LolStream a) where
   show l | not (isPeriodic l) = "LolStream[...]"
   show lol@(LolStream n l) = "LolStream" ++ show (destream lol)
```

>[!note]- `@` syntax for pattern matching
>`lol@(LolStream n l)` is a Haskell syntax for pattern matching that also binds the matched value to a name. This is called an "as pattern" in Haskell.
> 
> In this case, `lol@(LolStream n l)` is matching a value of the `LolStream` type, which consists of an `Int` and a list. The `n` and `l` are bindings for the `Int` and list inside the `LolStream`, respectively. The `lol@` part means that the entire `LolStream` value is also bound to the name `lol`.
> 
> This is useful when you want to use or return the entire value in some cases, but also need to work with its individual components in others. The as-pattern lets you do both without having to write separate pattern matches for each case.


