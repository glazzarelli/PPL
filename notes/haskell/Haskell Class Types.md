Haskell has a variety of built-in type classes that define common behaviors for different types. Here are some of the most commonly used type classes:

1. `Eq`: This type class is used for types that support equality testing. The functions its members implement are `==` and `/=`. If a type is a part of the `Eq` typeclass, it means that we can use the `==` and `/=` functions with values of that type [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

2. `Ord`: This type class is used for types that have an ordering. All types covered so far except for functions are part of `Ord`. `Ord` covers all the standard comparing functions such as `>`, `<`, `>=` and `<=`. The `compare` function takes two `Ord` members of the same type and returns an ordering. To be a member of `Ord`, a type must first have membership in the `Eq` class [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

3. `Show`: The `Show` type class is for types that can be converted to strings. The `show` function takes a value whose type is a member of `Show` and presents it to us as a string [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

4. `Read`: The `Read` type class is for types that can be converted from a string. The `read` function takes a string and returns a type which is a member of `Read` [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

5. `Enum`: Members of `Enum` are sequentially ordered types — they can be enumerated. The main advantage of the `Enum` typeclass is that we can use its types in list ranges. They also have defined successors and predecessors, which you can get with the `succ` and `pred` functions [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

6. `Bounded`: The `Bounded` type class is for types that have an upper and a lower bound [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

7. `Num`: The `Num` is a numeric typeclass. Its members have the property of being able to act like numbers. It includes all numbers, including real numbers and integral numbers. To join `Num`, a type must already be friends with `Show` and `Eq` [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

8. `Integral`: The `Integral` typeclass includes only integral (whole) numbers. In this typeclass are `Int` and `Integer` [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

9. `Floating`: The `Floating` typeclass includes only floating point numbers, so we have `Float` and `Double` as its instances [Source 7](http://www.learnyouahaskell.com/types-and-typeclasses).

Remember that you can also create your own type classes in Haskell. For example, you might create a `Foo` type class with a `foo` method that works with different types [Source 9](https://stackoverflow.com/questions/48442058/haskell-generic-typeclass-with-list).

When you declare a type class, you can specify a set of functions or operators that must be implemented for a type to be considered an instance of that type class. You can then create instances of your type class for any type that can implement those functions or operators. This is how Haskell achieves polymorphism: you can write functions that work on any type that is an instance of a particular type class, without knowing what the actual type is [Source 1](http://learnyouahaskell.com/Making-our-own-types-and-typeclasses).