
## String operations

Working with strings in Erlang involves using various functions provided by the `string` module. Here are some common operations:

1. **Creating Strings**: In Erlang, strings are represented as lists of integers where each integer represents a Unicode character. You can create a string like this:

   ```erlang
   1> String = "Hello, World!".
   "Hello, World!"
   ```

2. **Concatenating Strings**: You can concatenate strings using the `++` operator:

   ```erlang
   2> "Hello, " ++ "World!".
   "Hello, World!"
   ```

3. **Getting the Length of a String**: The `length/1` function returns the number of characters in a string:

   ```erlang
   3> length("Hello, World!").
   13
   ```

4. **Substrings**: Extracting substrings can be done using list slicing:

   ```erlang
   4> Substring = "Hello, World!" -- "World!".
   "Hello, "
   ```

5. **Replacing Parts of a String**: You can replace parts of a string using the `string:replace/3` function:

   ```erlang
   5> string:replace("Hello, World!", ", World!", "!").
   "Hello!"
   ```

6. **Splitting a String**: The `string:split/2` function splits a string into a list of substrings:

   ```erlang
   6> string:split("Hello, World!", ",").
   ["Hello", " World!"]
   ```

7. **Checking if a String Starts or Ends with a Specific Substring**: The `string:starts_with/2` and `string:ends_with/2` functions check if a string starts or ends with a specific substring:

   ```erlang
   7> string:starts_with("Hello, World!", "Hello").
   true
   8> string:ends_with("Hello, World!", "World!").
   true
   ```

8. **Case Conversion**: The `string:lowercase/1` and `string:uppercase/1` functions convert a string to lower case or upper case:

   ```erlang
   9> string:lowercase("Hello, World!").
   "hello, world!"
   10> string:uppercase("Hello, World!").
   "HELLO, WORLD!"
   ```

9. **Trimming Whitespace**: The `string:trim/1` function removes leading and trailing whitespace:

   ```erlang
   11> string:trim(" Hello, World! ").
   "Hello, World!"
   ```

These are just some of the basic operations you can perform with strings in Erlang. For more complex operations such as regular expressions, pattern matching, and more, refer to the official Erlang documentation for the `string` module [Source 0](https://www.erlang.org/doc/man/string), [Source 1](https://www.erlang.org/docs/17/man/string).

## Strings are Lists

Yes, in Erlang, a string is essentially a list of integers where each integer represents a Unicode character. This means that a string "hello" is shorthand for the list `[$h,$e,$l,$l,$o]` or `[104,101,108,108,111]`. Two adjacent string literals are concatenated into one during compilation, which doesn't incur any runtime overhead [Source 1](https://www.erlang.org/doc/reference_manual/data_types.html).

However, it's important to note that while strings in Erlang are implemented as lists of integers, they are not considered a separate data type. When you define a string in Erlang, it's actually a list of Unicode codepoints [Source 2](https://www.erlang.org/doc/man/string).

In fact, you can check if something is a string (which is a list of integers) by checking if all elements in the list are integers. Here's a simple function that checks if a given term is a string:

```erlang
is_string(X) ->
   lists:all(fun is_integer/1, X).
```

This function uses the `lists:all/2` function to check if all elements in the list `X` satisfy the condition specified by the function `fun is_integer/1`. If all elements are integers, the function returns `true`, indicating that `X` is a string. Otherwise, it returns `false` [Source 4](https://lethain.com/distinguishing-strings-from-lists-in-erlang/).