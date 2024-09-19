In Erlang, tuples are defined as a compound data type with a fixed number of terms, which are called elements. The number of elements is the size of the tuple. Tuples are enclosed in curly braces `{}`. 

Here's an example of defining a tuple in Erlang:

```erlang
P = {john, 24, {june, 25}}.
```

This tuple `P` contains three elements: the atom `john`, the integer `24`, and another tuple `{june, 25}`. 

You can use the `tuple_size` function to get the size of a tuple:

```erlang
tuple_size(P).
```

This will return `3`, the number of elements in the tuple `P`.

Additionally, you can perform other operations on tuples, such as:

- `is_tuple`: To determine if the term provided is a tuple.
- `list_to_tuple`: To convert a list to a tuple.
- `tuple_to_list`: To convert a tuple to a list.

Here's an example of using `tuple_to_list`:

```erlang
tuple_to_list({1,2,3}).
```

This will convert the tuple `{1,2,3}` to a list `[1,2,3]`.

It's important to note that tuples in Erlang are indexed starting at 1, and you can access elements using the `element` function and you can create a new tuple with an updated value using `setelement`. For example:

```erlang
element(2, P).
```

This will return `24`, which is the second element of the tuple `P`.

To update the second element of the tuple `P` to `25`, you can do:

```erlang
P2 = setelement(2, P, 25).
```

Now `P2` will be `{john, 25, {june, 25}}`.

For more detailed information on tuples in Erlang, you can refer to the Erlang documentation on data types [here](https://www.erlang.org/doc/reference_manual/data_types.html) and programming examples [here](https://www.erlang.org/doc/programming_examples/records).