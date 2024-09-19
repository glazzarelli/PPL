In Erlang, a case expression is used for pattern matching. It allows you to evaluate different expressions based on the pattern of the input. Here's a breakdown of the syntax and some examples covering different cases:

### Syntax:
```erlang
case Expression of
    Pattern1 [when Guard1] ->
        Expression1;
    Pattern2 [when Guard2] ->
        Expression2;
    ...
    PatternN [when GuardN] ->
        ExpressionN
end.
```

- `Expression`: The value being matched.
- `Pattern`: The pattern to match against the expression.
- `Guard`: An optional additional condition to check for each pattern.
- `ExpressionN`: The expression to be evaluated if the corresponding pattern matches.

### Examples:

1. **Matching a Constant Value**:
```erlang
Value = 42,
case Value of
    42 ->
        io:format("The answer to life, the universe, and everything.~n");
    _ ->
        io:format("This is not the answer.~n")
end.
```

2. **Matching Variables**:
```erlang
Value = 42,
case Value of
    X when X > 0 ->
        io:format("Positive number~n");
    X when X < 0 ->
        io:format("Negative number~n");
    _ ->
        io:format("Zero~n")
end.
```

3. **Matching Tuples**:
```erlang
Tuple = {ok, "Hello"},
case Tuple of
    {ok, Message} ->
        io:format("Received message: ~s~n", [Message]);
    {error, Reason} ->
        io:format("Error occurred: ~p~n", [Reason])
end.
```

4. **Matching Lists**:
```erlang
List = [1, 2, 3],
case List of
    [First | Rest] ->
        io:format("First element is ~w, and rest is ~w~n", [First, Rest]);
    [] ->
        io:format("List is empty~n")
end.
```

5. **Using Guards**:
```erlang
Number = 15,
case Number of
    N when N > 10, N < 20 ->
        io:format("Number ~w is between 10 and 20~n", [N]);
    N when N > 20 ->
        io:format("Number ~w is greater than 20~n", [N]);
    _ ->
        io:format("Number ~w is less than or equal to 10~n", [N])
end.
```

6. **Using Wildcard (`_`)**:
```erlang
Value = "hello",
case Value of
    "world" ->
        io:format("Matched 'world'~n");
    _ ->
        io:format("Didn't match 'world'~n")
end.
```

These examples cover various scenarios of using the `case` expression in Erlang, including matching constants, variables, tuples, lists, using guards, and employing the wildcard for default cases.