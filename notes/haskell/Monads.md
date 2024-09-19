Monads in Haskell are a type of abstract data type used to manage side effects. They're a way to structure programs and handle things like I/O operations, exceptions, state, or computations that can fail, in a way that integrates neatly with the pure functional logic of Haskell. Monads provide a way to sequence operations, manage side effects, and encapsulate computations with shared state.

In essence, a Monad is a type that defines some computation step, and also describes how to combine these steps in a pipeline. This is done through two main operations: `return` and `bind (>>=)`.

- `return`: This operation takes a value and puts it into a minimal context that satisfies the laws of the monad. It's not the same as the return in many other languages. Instead, it creates a monadic value out of a normal value.

- `bind (>>=)`: This operation, pronounced "bind", is used to chain together monadic operations. It takes a monadic value (a value within the monad context) and a function that takes a normal value and returns a monadic value, and feeds the monadic value into the function.

Here is an example of these operations for the `Maybe` monad:

```haskell
return :: a -> Maybe a
return x = Just x

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>= func = Nothing
(Just x) >>= func = func x
```
[Source 0](https://wiki.haskell.org/All_About_Monads)

One of the reasons we need Monads in Haskell is because it's a purely functional language and doesn't allow side effects. Monads provide a way to handle these effects in a controlled and predictable manner. They encapsulate the side effects and manage them, allowing the rest of the Haskell code to remain pure.

A classic example of a monad is the `IO` monad, which encapsulates any computation that does I/O. Here's an example:

```haskell
main :: IO ()
main = do
    putStrLn "Enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")
```
In this example, `putStrLn` and `getLine` are both `IO` actions. The `do` notation is a syntactic sugar for chaining monadic operations using the bind `(>>=)` operator. Each line within the `do` block is a separate action within the `IO` monad. The `name <- getLine` line is using the result of the `getLine` action in the next `IO` action.

While this is a basic overview, monads can become quite complex, particularly when you start using monad transformers to stack multiple monads. But at their heart, they're about sequencing operations and managing side effects in a way that integrates with Haskell's pure functional logic [Source 0](https://wiki.haskell.org/All_About_Monads) [Source 3](https://en.wikibooks.org/wiki/Haskell/Understanding_monads).
