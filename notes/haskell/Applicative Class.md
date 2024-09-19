The `Applicative` class in Haskell is a typeclass that represents an "applicative functor". It provides a way to apply functions that are wrapped inside a functor to values that are also wrapped inside a functor. It's defined in the `Control.Applicative` module and is part of the standard library [Source 0](http://www.learnyouahaskell.com/functors-applicative-functors-and-monoids).

The class definition is as follows:

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Here, `f` is a type constructor that represents an applicative functor. The `pure` function takes a value of any type and returns an applicative functor with that value inside it, effectively "wrapping" the value in the functor. The `<*>` function is used to apply a function that is inside a functor to a value that is inside a functor [Source 0](http://www.learnyouahaskell.com/functors-applicative-functors-and-monoids).

The `<*>` function is particularly interesting because it extends the idea of mapping a function over a functor (as with `fmap` in the `Functor` class) to mapping a function that is itself inside a functor. This allows us to apply functions that are wrapped in a functor to values that are also wrapped in a functor [Source 0](http://www.learnyouahaskell.com/functors-applicative-functors-and-monoids).

To create an instance of `Applicative` for a custom data type `ZL a` (zip list of type a) which is defined as:

```haskell
data ZL a = ZEmpty | ZCons a (ZL a) deriving Eq
```

We first need to make `ZL a` an instance of `Functor` because `Applicative` is a subclass of `Functor`. Then we can define the `pure` and `<*>` functions for `ZL a`. The `pure` function should take a value and return a `ZL a` with that value inside it, and the `<*>` function should take a `ZL a` that contains a function and another `ZL a`, and return a new `ZL a` that results from applying the function to the value. For example:

```haskell
instance Functor ZL where
    fmap _ ZEmpty = ZEmpty
    fmap f (ZCons x xs) = ZCons (f x) (fmap f xs)

instance Applicative ZL where
    pure x = ZCons x ZEmpty
    ZEmpty <*> _ = ZEmpty
    _ <*> ZEmpty = ZEmpty -- no need if (f ZEmpty) is implemented
    (ZCons f fs) <*> xs = fmap f xs `append` (fs <*> xs)

append :: ZL a -> ZL a -> ZL a
append ZEmpty ys = ys
append (ZCons x xs) ys = ZCons x (append xs ys)
```

In this example, we define the `fmap` function to apply a function to each element in a `ZL a`, and the `pure` function to take a value and return a `ZL a` with that value. The `<*>` function applies each function in the first `ZL a` to each value in the second `ZL a`, and combines the results using the `append` function, which concatenates two `ZL a` [Source 4](https://stackoverflow.com/questions/51989707/making-a-basic-haskell-type-an-instance-of-a-new-typeclass).