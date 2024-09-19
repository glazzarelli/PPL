## What's an instance in Haskell?

An instance declaration has the form:
```
instance (assertion1, ..., assertionN) => class type1 ... typem where ...
```
The part before the "`=>`" is the _context_, while the part after the "`=>`" is the _head_ of the instance declaration.
The assertions in the *context* of the instance declaration must be of the form `C a` where `a` is a type variable that occurs in the head.

In Haskell, an "instance" refers to a particular implementation of a typeclass for a specific type. It's the part where you define how the typeclass's methods should behave for that type. A typeclass, by analogy to object-oriented programming, is like an interface that specifies a set of functions or methods but not their implementations. The implementations are provided by instances for specific types.

To define an instance for a type with multiple type parameters, you can still use a single typeclass, but you would have to specify how the typeclass's functions should work with all the type parameters involved. Here's an example using the `Show` typeclass:

```haskell
data Pair a b = Pair a b

instance (Show a, Show b) => Show (Pair a b) where
  show (Pair x y) = "Pair " ++ show x ++ " " ++ show y
```

In this example, `Pair` is a type with two type parameters, `a` and `b`. The instance declaration specifies that `Pair a b` can be an instance of `Show` if both `a` and `b` are also instances of `Show`. This means that `show` will be able to convert both elements of the pair to a string.

## Functor instance of Fancy Pair

The `Functor` instance in the provided snippet defines how the `fmap` function should be applied to the `Fpair` data structure. `Fpair` is a data type with two constructors, `Fpair` and `Pair`, each taking a different number of arguments. Here's the definition and the `Functor` instance:

```haskell
data Fpair s a = Fpair a a s | Pair a a

instance Functor (Fpair s) where
  fmap f (Fpair x y t) = Fpair (f x) (f y) t
  fmap f (Pair x y) = Pair (f x) (f y)
```

The `Functor` typeclass requires a single method `fmap`, which takes a function and a functor, and applies the function to the contents of the functor. In this case, the `fmap` function is defined for `Fpair s` where `s` is a type that is not affected by the mapping—only the `a` type is transformed.

For the `Fpair` constructor, `fmap` applies the function `f` to both `x` and `y`, which are of type `a`, and leaves the `t` value, which is of type `s`, unchanged. This aligns with the functorial property of mapping a function over the contents of the functor without altering its structure.

For the `Pair` constructor, `fmap` similarly applies the function `f` to both `x` and `y` and does not have a `s` type to consider.

The `Functor` instance for `Fpair` adheres to the functor laws:
- Identity: `fmap id` should be equivalent to `id`.
- Composition: `fmap (f . g)` should be equivalent to `fmap f . fmap g`.

By defining the `fmap` function in this way, it ensures that these laws are maintained, as the structure of the `Fpair` remains the same after the function application, and only the `a` values are transformed [Source 0](https://wiki.haskell.org/Functor).

---

***Note***: if try to change the order of the parameters in the datatype definition we obtain:
```haskell
data Fpair s a = Fpair a a s | Pair a a

instance Functor (Fpair _ s) where
  fmap f (Fpair x y t) = Fpair (f x) (f y) t
  fmap f (Pair x y) = Pair (f x) (f y)
```
Please note that using wildcards in instance declarations is not standard Haskell and requires the `FlexibleInstances` language extension. The correct approach without language extensions would be to define a newtype wrapper that restricts the type to the element of interest, or to define a class that captures the behavior for the specific element you are interested in.