## Erlang

Erlang is a concurrent-oriented programming language that was initially developed for telecommunication applications, such as switches. It is known for its transparent distribution and robustness. Here are some key points about Erlang:
1. **Concurrent-Oriented**: Erlang is designed for concurrent programming, making it well-suited for applications where many processes run simultaneously. Its processes are lightweight and are managed by the Erlang Virtual Machine (BEAM).
2. **Functional Core**: While Erlang's core is functional, it's not as purely functional as Haskell. Instead, it focuses on practicality, which suits its industrial use cases.
3. **Syntax Influenced by Prolog**: Erlang's syntax is influenced by its original Prolog implementation, which gives it a unique and declarative structure.
4. **Dynamic Typing**: Similar to Scheme, Erlang uses dynamic typing, which means variable types are determined at runtime.
5. **OTP Library**: Erlang comes with a solid standard library called OTP (Open Telecom Platform), designed for building distributed and fault-tolerant applications. OTP supports continuously running applications and updates with code swaps.

Erlang runs on the Erlang Virtual Machine (BEAM), which is known for its robustness and support for parallel and distributed systems. Other languages, like Elixir and LFE, also run on the BEAM, allowing for a range of programming options.

Despite not being mainstream, Erlang has found applications in industrial settings, including WhatsApp, Call of Duty servers, Amazon's SimpleDB, Yahoo's Delicious, Facebook Chat, and Pinterest (which uses Elixir). Additionally, its principles have influenced the development of new languages and frameworks, making robust distributed computing more relevant than ever.

### Sequential Programming

#### Basics

Erlang's syntax includes conventions for variables, atoms, tuples, lists, pattern matching, maps, and function calls.

***Variables***: In Erlang, variables start with an uppercase letter, much like in Prolog. These variables can only be bound once, meaning their values can't be changed after they have been set.
```erlang
Abc 
A_long_variable_name 
ACamelCaseVariableName
```


***Atoms***: Atoms in Erlang are similar to symbols in Scheme. They can consist of any character code, and singly quoted sequences of characters are considered atoms. Unquoted atoms must be lowercase to avoid clashes with variables:
```erlang
abcef 
start_with_a_lower_case_letter 
'Blanks can be quoted'
'Anything inside quotes \n'
```


***Tuples***: Tuples are used to store a fixed number of items in Erlang. They are represented with curly braces, and they can contain various data types, including other tuples.
```erlang
{123, bcd} 
{123, def, abc} 
{person, ’Jim’, ’Austrian’} % three atoms! 
{abc, {def, 123}, jkl}
```


***Lists***: Lists in Erlang are similar to those in Haskell, and you can use the `++` operator for concatenation. Erlang also has a cons operator `[X | L]`, similar to `(cons X L)` in other functional languages. Lists are strings, like in Haskell, but is getting common to use bitstrings and UTF.

**Comprehensions** are more or less like in Haskell: 
```erlang
> [{X,Y} || X <- [-1, 0, 1], Y <- [one, two, three], X >= 0]. 
[{0,one},{0,two},{0,three},{1,one},{1,two},{1,three}]
```


***Pattern Matching***: Pattern matching is a fundamental concept in Erlang, similar to Prolog. The `=` operator is used for pattern matching. You can use `_` as a "don't care" variable when you don't need to bind a value.
```erlang
A = 10 
% Succeeds - binds A to 10 
{A, A, B} = {abc, abc, foo} 
% Succeeds - binds A to abc, B to foo 
{A, A, B} = {abc, def, 123} 
% Fails 
[A,B|C] = [1,2,3,4,5,6,7] 
% Succeeds - binds A = 1, B = 2, C = [3,4,5,6,7] 
[H|T] = [abc] 
% Succeeds - binds H = abc, T = [] 
{A,_, [B|_], {B}} = {abc, 23, [22, x], {22}} 
% Succeeds - binds A = abc, B = 22
```

>[!help]- Explanation
>`{A, A, B} = {abc, def, 123}`  fails because when you repeat a variable in a pattern, Erlang expects the same value in each place where the variable appears, since variables can only be bound once. This is why your snippet fails: `A` is bound to `abc` in the first position, but the second `A` in the pattern does not match `def` in the tuple.
>
>The `|` operator in the pattern `[A,B|C]` is used to split the list into its head (the first two elements in this case) and its tail (the rest of the list). This is a common idiom in Erlang and other languages in the same family (like Elixir) for working with lists
>
>In the last example:
> - `[B|_]` is a pattern that matches a list with at least one element. `B` is matched with the first element of the list `[22, x]`, so `B` is now bound to `22`. The rest of the list is ignored because of the `_` in the pattern.
> - `{B}` is a pattern that matches a tuple with one element, and that element should match the value `B` is currently bound to (which is `22`). This pattern successfully matches the tuple `{22}`, so the overall pattern match succeeds.

***Maps***: Erlang has a relatively new data structure called maps, which are similar to hash tables. You can perform various operations on maps, such as updating or inserting key-value pairs.
```erlang
> Map = #{one => 1, "Two" => 2, 3 => three}. 
#{3 => three,one => 1,"Two" => 2} 

% update/insert 
> Map#{one := "I"}. 
#{3 => three,one => "I","Two" => 2} 

> Map. 
#{3 => three,one => 1,"Two" => 2} % unchanged 

% I want the value for "Two": 
> #{"Two" := V} = Map. 
#{3 => three,one => 1,"Two" => 2} 

> V. 
2
```

>[!help]- Explanation
> 1. `Map = #{one => 1, "Two" => 2, 3 => three}` defines a map with three key-value pairs.
> 2. `Map#{one := "I"}` updates the value for the key `one` in the map. The `:=` operator is used to update an existing key in a map. If the key does not exist in the map, this operation will fail.
> 3. `Map` displays the original map. Note that maps in Erlang are immutable, which means the original map is not changed when you update a key. The update operation actually creates a new map with the updated key-value pair.
> 4. `#{"Two" := V} = Map` extracts the value for the key `"Two"` from the map and binds it to the variable `V`.

***Function Calls***: you call functions using the `module:func(Arg1, Arg2, ... Argn)` syntax. Functions must be defined within modules and exported to be called from outside the module. Function and module names must be atoms.
You can use the `-import` directive to avoid qualified names, it is discouraged:
```erlang
- import(module_name, [func/n])
func(Arg1, Arg2, .. Argn)
```

A better approach is:
```erlang
% better alternative
module_name:func(Arg1, Arg2, ... Argn)
```

***Module System***: Erlang uses a module system to organize code. Modules are defined using the `-module(ModuleName).` directive, and functions are exported using `-export([Function/Arity]).`:
```erlang
-module(demo). 
-export([double/1]). 
double(X) -> 
  times(X, 2). 
times(X, N) -> 
  X * N.
```

You can open the interactive shell with the command `erl`:
```erlang
1> c(demo).     
{ok,demo}             % demo.erl file is compiled 
2> demo:double(25).
50
3> demo:times(4,3).
** exception error: undefined function demo:times/2
```


>[!example]
> A working example of an Erlang program:
>
>```erlang
> -module(main).
> -import(io,[fwrite/2]).
> -export([start/0]).
> 
> start() ->
>    List = [{X,Y} || X <- [-1, 0, 1], Y <- [one, two, three], X >= 0],
>    fwrite("~p\n", [List]).
> ```
> 
> - The `-module(main).` directive is used to define the name of the module, which in this case is `main`. This is necessary for Erlang to know where to look for the functions that you're defining
> - The `-import(io,[fwrite/2]).` directive is used to import the `fwrite/2` function from the `io` module. After this line, you can use `fwrite` without the `io:` prefix. However, using `-import` is generally discouraged in Erlang community because it can cause confusion, instead, you can simply use `io:fwrite` without importing it. This is the recommended way to use built-in functions in Erlang. It's clear and explicit about which module the function comes from.
> - The `-export([start/0]).` directive is used to make the `start/0` function available to other modules. Without this line, the `start` function would be private to the `main` module and couldn't be called from outside the module
> - This is the definition of the `start/0` function. It consists of two main parts:
> 	1. `List = [{X,Y} || X <- [-1, 0, 1], Y <- [one, two, three], X >= 0]`: This is a list comprehension, which generates a list of tuples. Each tuple contains a number from the list `[-1, 0, 1]` and a word from the list `[one, two, three]`, but only if the number is greater than or equal to 0.
> 	2. `fwrite("~p\n", [List])`: This line uses the `fwrite/2` function from the `io` module (which was imported earlier) to print the `List` to the standard output. The "~p\n" format string tells `fwrite/2` to print `List` in a pretty format followed by a newline
>   
> The `/` at the end of function names in Erlang signifies the ***arity*** of the function. Arity refers to the number of arguments a function takes.
> This notation is common in Erlang and is useful for distinguishing between different versions of a function that take different numbers of arguments. For example, `fwrite/1` and `fwrite/2` would be considered two distinct functions in Erlang. The first function, `fwrite/1`, takes one argument, while the second function, `fwrite/2`, takes two arguments.
> The combination of a function's name and its arity is known as a function's signature and this uniquely identifies a function within a module. This means you can have multiple functions with the same name in the same module, as long as they have different arities.
> 
> In Erlang, the ***entry point*** of the program is not defined by a specific module or function name but by the function that is first called when the program is run. If you have multiple modules and multiple functions in your Erlang program, the entry point is the function that you first invoke when you start the program.
> When you compile and run your Erlang program, you specify the module and function to run. For instance, if you have an Erlang file named `my_program.erl` with a module named `my_module` and a function named `my_function`, you would compile the file with `c(my_program).` in the Erlang shell, then run the function with `my_module:my_function().` This would make `my_function/0` in `my_module` the entry point of your program.
> It's important to note that in Erlang, all functions are local to the module they are defined in, unless they are exported using the `-export` directive. If a function is not exported, it cannot be called from outside of its module. Therefore, your entry point function must be exported from its module.
> It is common practice in Erlang to use `start/0` or `init/0` as the entry point function, due to their descriptive nature.
> 
> Remember to compile and run your Erlang modules. You can use the `erlc` command to compile your module:
> ```sh
> erlc main.erl
> ```
> Alternative:
> ```sh
> elc -compile main.erl  
> ```
> 
> And then you can use the `erl` command to start the Erlang shell and run your `start/0` function:
> ```sh
> erl -noshell -s main start -s init stop
> ```
> The `-noshell` option starts Erlang in a non-interactive mode, `-s main start` calls the `start/0` function from the `main` module, and `-s init stop` stops Erlang after your function has finished executing.

#### Built-in functions

In the Erlang programming language, **Built-In Functions (BIFs)** are part of the Erlang module and are generally implemented in C. They are used to perform operations that are either impossible, complex, or inefficient to implement in Erlang itself. Some examples of BIFs are as follows:
- `date()`: Returns the current date.
- `time()`: Returns the current time.
- `length([1,2,3,4,5])`: Returns the length of the list.
- `size({a,b,c})`: Returns the size of the tuple.
- `atom_to_list(an_atom)`: Converts an atom to a list.
- `list_to_tuple([1,2,3,4])`: Converts a list to a tuple.
- `integer_to_list(2234)`: Converts an integer to a list.
- `tuple_to_list({})`: Converts a tuple to a list.

```erlang
-module(helloworld). 
-export([start/0]). 
start() ->   
   io:fwrite("~p~n",[tuple_to_list({1,2,3})]), 
   io:fwrite("~p~n",[time()]).

% [1,2,3]
% {8,19,50}
```
In this example, the BIF `tuple_to_list` is used to convert a tuple to a list, and the BIF `time` is used to output the system time.

Erlang functions are defined as a sequence of clauses. The clauses are scanned sequentially until a match for the given parameters is found. When a match is found, all variables in the head of the clause become bound. Variables are local to each clause and are handled automatically by Erlang. The body of the clause is then evaluated sequentially. 

A simple Erlang function could be written as follows:
```erlang
-module(mathStuff).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
```
In this example, the function `factorial` is defined with two clauses. The first clause matches when the input is `0`, and the second clause matches for any other integer `N`. The `factorial` function is not *tail-recursive* because a multiplication is performed after the recursive call to `factorial(N-1)`.
A tail-recursive alternative:
```erlang
factorial(N) -> factorial(N, 1).

factorial(0, Acc) -> Acc;
factorial(N, Acc) when N > 0 -> factorial(N-1, N*Acc).
```

#### Guard clauses

***Guard clauses*** can be added to functions to control their execution based on certain conditions. The keyword `when` is used to introduce a guard. There is a guard sub-language in Erlang because guards must be evaluated in constant time. This means that custom predicates cannot be used within them. Here is an example of a function with a guard clause:
```erlang
temperature(Temp) when Temp >= 100 ->
    boiling;
temperature(Temp) when Temp =< 0 ->
    freezing;
temperature(_) ->
    normal.
```

Examples of guard expressions:
- `number(X)`: X is a number
- `integer(X)`: X is an integer
- `float(X)`: X is a float
- `atom(X)`: X is an atom
- `tuple(X)`: X is a tuple
- `list(X)`: X is a list
- `X > Y + Z`: X is greater than Y + Z
- `X =:= Y`: X is exactly equal to Y
- `X =/= Y`: X is not exactly equal to Y
- `X == Y`: X is equal to Y (with int coerced to floats, i.e., 1 == 1.0 succeeds, but 1 =:= 1.0 fails)
- `length(X) =:= 3`: X is a list of length 3
- `size(X) =:= 2`: X is a tuple of size 2.
All variables in a guard must be bound.

> [!note] Guard Sub-Language Restrictions
The guard sub-language in Erlang includes only a limited set of expressions that are known to be safe and efficient. These include:
> 
> - Comparison operators: `==`, `/=`, `=<`, `<`, `>=`, `>`
> - Boolean operators: `and`, `or`, `not`
> - Arithmetic operators: `+`, `-`, `*`, `/`
> - Type test functions: `is_atom/1`, `is_integer/1`, `is_float/1`, `is_list/1`, etc.
> - Other safe functions: `abs/1`, `length/1`, `element/2`, etc.

#### Apply syntax

The `apply` function in Erlang allows you to apply a function to a list of arguments. The syntax for `apply` is as follows:
```erlang
apply(Mod, Func, Args)

apply(io, format, ["Hello, ~p~n", ["World"]]) % prints "Hello, World"

% calls the function `some_function` in the current module with arguments `Arg1` and `Arg2`.
apply(?MODULE, some_function, [Arg1, Arg2])
```
where `Mod` is the module name, `Func` is the function name, and `Args` is the list of arguments. Both `Mod` and `Func` need to be atoms, or expressions that evaluate to atoms. Any Erlang expression can be used in the arguments to `apply`. The `?MODULE` macro can be used to get the name of the current module.

#### Higher-order functions

**Lambda functions** are defined using the `fun` keyword. The syntax for defining a lambda function is `fun(Parameters) -> Body end`. For example, a function that squares a number can be defined as follows:
```erlang
Square = fun(X) -> X*X end.
```
This lambda function can then be called like any other function. For example, `Square(3)` will return `9`. 
```erlang
Result = Square(3). % Result will be 9
```

Lambda functions can also be passed as parameters to **higher-order functions**. Higher-order functions are functions that take other functions as arguments or return them as results.

An example of using a lambda function with a higher-order function is the `lists:map` function, which applies the given function to each element in the list:
```erlang
ResultList = lists:map(Square, [1,2,3]). % ResultList will be [1, 4, 9]
```
In this case, the `Square` function is applied to each element in the list `[1,2,3]`, resulting in the list `[1,4,9]`.

To pass standard (i.e., "non-lambda") functions to higher-order functions, you need to prefix their name with `fun` and state their arity. For example, to pass a function `my_function` with arity 2 to the `lists:foldr` function, you would write:
```erlang
ResultFold = lists:foldr(fun my_function/2, 0, [1,2,3]).
```
`my_function/2` is the function being passed to `lists:foldr`. The `0` is the initial accumulator value for the fold operation, and `[1,2,3]` is the list being folded.

Lambda functions in Erlang can access variables from their surrounding scope. This is known as a closure. For example:
```erlang
base(A) ->
    B = A + 1,
    F = fun() -> A * B end,
    F().

% base(5) => 30 
```
The lambda function `F` can access the variables `A` and `B` from its surrounding scope in the `base` function. However, variables defined within the lambda function are not accessible outside of it.

### Concurrent Programming: The Actor Model

The ***Actor Model*** was introduced by Carl Hewitt, Peter Bishop, and Richard Steiger in 1973. It's a conceptual model to deal with concurrent computation. The fundamental building blocks of the Actor Model are actors, which are independent units of computation. Here are some key characteristics of the Actor Model:
- Everything is an actor: an independent unit of computation.
- Actors are inherently concurrent.
- Actors communicate only through messages (asynchronous communication).
- Actors can be created dynamically.
- The order of received messages is not guaranteed.

Erlang is a functional programming language designed for building scalable and maintainable applications. It is renowned for its simplicity and efficiency in writing concurrent programs. Here are some key features of Erlang:
- Writing concurrent programs is easy and efficient.
- Concurrency can be taken into account at early stages of development.
- Processes are represented using different actors communicating only through messages.
- Each actor is a lightweight process, handled by the Erlang Virtual Machine (VM). It is not mapped directly to a thread or a system process, and the VM schedules its execution.
- The VM handles multiple cores and the distribution of actors in a network.
- Creating a process is fast, and highly concurrent applications can be faster than the equivalent in other programming languages. 

#### Main Primitives for Concurrent Programming

Erlang provides three main primitives for concurrent programming:
1. `spawn`: This function creates a new process executing the specified function, returning an identifier. For example, `Pid2 = spawn(Mod, Func, Args)` creates a new process and assigns its process identifier (Pid) to `Pid2`.
2. `send` or `!`: This operator sends a message to a process through its identifier. The content of the message is simply a variable. The operation is asynchronous, which means the sender does not wait for the message to be received. For example, `PidB ! {self(), foo}` sends a message `{self(), foo}` to the process identified by `PidB`.
3. `receive ... end`: This construct extracts a message from a process's mailbox queue that matches with the provided set of patterns. This operation is blocking if no message is in the mailbox. The mailbox is persistent until the process quits. For example, `receive {From, Msg} -> Actions end` receives a message and binds it to `From` and `Msg`. Variables `From` and `Msg` become bound when receiving the message, if `From` is bound before receiving a message, then only data from that process is accepted.

The `self()` function is used to return the Pid of the process executing it. This is useful when a process needs to identify itself in a message.

A simple example of an echo process:
```erlang
-module(echo).
-export([go/0, loop/0]).

go() ->
    Pid2 = spawn(echo, loop, []),
    Pid2 ! {self(), hello},
    receive
        {Pid2, Msg} ->
            io:format("P1 ~w~n", [Msg])
    end,
    Pid2 ! stop.

loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.
```

>[!help]- Explanation
>1. `go() ->` function starts the echo server: it first spawns a new process running the `loop` function in the `echo` module (`Pid2 = spawn(echo, loop, [])`), then sends a message to this new process (`Pid2 ! {self(), hello}`).
> 2. `receive {Pid2, Msg} -> io:format("P1 ~w~n", [Msg]) end,` is used to handle incoming messages. It's saying "if I receive a message that's a tuple with `Pid2` as its first element, bind the second element of the tuple to `Msg` and print `Msg`."
> 3. `Pid2 ! stop.`: This sends a `stop` message to the process with the process ID `Pid2`.
> 4. `loop() ->`: This is the function that's run in the new process. It's a simple loop that waits for messages and responds to them.
> 5. `receive {From, Msg} -> From ! {self(), Msg}, loop(); stop -> true end.` is used to handle incoming messages in the new process. If it receives a tuple, it sends a reply back to the sender with a tuple containing its own process ID and the received message, then calls itself to continue the loop. If it receives `stop`, it returns `true` and ends the loop.
> 
> In other words, this code creates a simple echo server that echoes back any messages it receives until it gets a `stop` message:
> 

```erlang
1> c(echo).
{ok,echo}
2> echo:go().
P1 hello
stop
```

#### Selective Message Reception

In Erlang, a process can selectively receive messages based on their content. For example, a process can perform:
```erlang
receive
    foo ->
        true
end,

receive
    bar ->
        true
end.
``` 
to first receive a `foo` message and then a `bar` message, irrespective of the order in which they were sent.

A process can also select any message to process, regardless of its content. For example, `receive Msg -> ... ; end` will process the first message to arrive at the process, and bind the variable `Msg` to the content of the message.

#### Registered Processes

A process can be registered with a name using the `register(Alias, Pid)` function, where `Alias` is the name and `Pid` is the process identifier. Any process can send a message to a registered process using its name instead of its Pid.

Here's an example of starting a registered process:
```erlang
start() ->
    Pid = spawn(?MODULE, server, []),
    register(analyzer, Pid).

analyze(Seq) ->
    analyzer ! {self(), {analyze, Seq}},
    receive
        {analysis_result, R} ->
            R
    end.
```

#### Client-Server Model in Erlang

The Client-Server model can be easily realized in Erlang through a simple protocol, where requests have the syntax `{request, ...}`, while replies are written as `{reply, ...}`.

Here's an example of server code:
```erlang
-module(myserver).
server(Data) ->
    receive
        {From, {request, X}} ->
            {R, Data1} = fn(X, Data),
            From ! {myserver, {reply, R}},
            server(Data1)
    end.
```
Where `Data` represent the state of the server.

And here's an interface library for the client:
```erlang
-export([request/1]).
request(Req) ->
    myserver ! {self(), {request, Req}},
    receive
        {myserver, {reply, Rep}} -> Rep
    end.
```

#### Timeouts

Consider the following code snippet:
```erlang
receive
    foo ->
        Actions1;
    after Time ->
        Actions2
end.
```
In this code, if the process receives the message `foo` within the `Time` duration, it performs `Actions1`. If it doesn't receive the message within this time, it executes `Actions2`.

The `sleep(T)` function suspends the process for `T` milliseconds:
```erlang
sleep(T) ->
    receive
        after T ->
            true
    end.
```

The `suspend()` function suspends the process indefinitely.
```erlang
suspend() ->
    receive
        after infinity ->
            true
    end.
```

The `set_alarm(T, What)` function sets a timer to send a specific message to a process after a certain duration:
```erlang
set_alarm(T, What) ->
    spawn(timer, set, [self(), T, What]).

set(Pid, T, Alarm) ->
    receive
        after T -> Pid ! Alarm
    end.

receive Msg -> ... ; end
```
The message `What` is sent to the current process after `T` milliseconds.

You can also use a timeout of 0 to check the message buffer first and if it is empty, execute the following code:
```erlang
flush() ->
    receive
        Any -> flush()
    after 0 -> true
    end.
```
The `flush()` function is used in a recursive manner to continuously receive and discard any messages that may be in the process's message buffer. It doesn't perform any action on the messages, it just removes them from the buffer. This is useful when you want to clear the message queue of a process.

On the other hand, the `after 0 -> true` portion of the code is a timeout clause. In Erlang, the `receive` statement can have an optional timeout clause, which is executed if no messages are received within the specified time. In this case, the timeout is set to 0 milliseconds. This means that if the message buffer is empty (i.e., there are no messages to receive and thus `flush()`), the timeout clause is immediately triggered, and the function returns `true`.

So, in summary, `flush()` is used to clear out the message buffer, and the timeout clause is used to end the function when the message buffer is empty.

#### Building Reliable and Scalable Applications

Erlang provides the Open Telecom Platform (OTP), a set of libraries and design principles for building industrial applications. OTP includes ***behaviours*** (ready-to-use design patterns such as Server, Supervisor, Event manager, etc.) where only the functional part of the design has to be implemented (callback functions).

Erlang also supports the ***"let it crash"*** principle, where processes are allowed to fail and supervisors take care of restarting them. This allows for a self-healing system where errors are isolated and recovery is automatic.

Furthermore, Erlang supports hot code swapping, where application code can be loaded at runtime, and code can be upgraded without stopping the system. The processes running the previous version continue to execute, while any new invocation will execute the new code.

%% TODO: fix the example %%
Here's an example of a simple supervisor linked to a number of workers:
```erlang
-module(main).
-export([start/1]).

start(Count) ->
    register(the_master, self()), % I'm the master, now
    start_master(Count),
    unregister(the_master),
    io:format("That's all.~n").

start_master(Count) ->
    % The master needs to trap exits:
    process_flag(trap_exit, true),
    create_children(Count),
    master_loop(Count).
    
create_children(0) -> ok;
create_children(N) ->
    Child = spawn_link(?MODULE, child, [0]), % spawn + link
    io:format("Child ~p created~n", [Child]),
    Child ! {add, 0},
    create_children(N-1).
    
master_loop(Count) ->
    receive
        {value, Child, V} ->
            io:format("child ~p has value ~p ~n", [Child, V]),
            Child ! {add, rand:uniform(10)},
            master_loop(Count);
        {'EXIT', Child, normal} ->
            io:format("child ~p has ended ~n", [Child]),
            if Count =:= 1 -> ok;
               true -> master_loop(Count-1)
            end;
        {'EXIT', Child, _} ->
            % "unnormal" termination
            NewChild = spawn_link(?MODULE, child, [0]),
            io:format("child ~p has died, now replaced by ~p ~n", [Child, NewChild]),
            NewChild ! {add, rand:uniform(10)},
            master_loop(Count)
    end.
    
child(Data) ->
    receive
        {add, V} ->
            NewData = Data + V,
            BadChance = rand:uniform(10) < 2,
            if
                % random error in child
                BadChance -> error("I'm dying");
                % child ends naturally
                NewData > 30 -> ok;
                % there is still work to do
                true -> the_master ! {value, self(), NewData}, child(NewData)
            end
    end.
```
- In the `main` function, it registers itself as the master, starts the master with a certain number of workers, unregisters itself, and then prints "That's all.".
- In the `start_master` function, it sets the process flag to trap exits, creates children processes, and then enters a loop.
- The `trap_exit` flag is used to control how a process handles exit signals from its linked or monitored child processes. When `trap_exit` is set to `true` for a process, it means that the process will "trap" exit signals, and it can handle them without terminating itself. This is particularly useful for processes that supervise other processes, such as `the_master`.
- In the `create_children` function, it spawns and links children processes, sends an `add` message to each child, and recursively creates the remaining children.
- `spawn_link/3` is a function from the Erlang standard library that creates a new process and links it to the current process. Links are bidirectional connections between processes. If a process that is linked to another process terminates abnormally (i.e., it crashes), the linked process will also be terminated, unless it is trapping exits. This behavior is part of Erlang's "let-it-crash" philosophy, which encourages developers to let processes crash and restart cleanly, rather than trying to handle every possible error condition. `[0]` is the list of arguments passed to the `child` function when the new process is spawned.
- In the `master_loop` function, it receives messages from its children. If a child sends a `value` message, it prints the child's value, sends an `add` message back to the child, and continues the loop. If a child exits normally, it prints a message and ends the loop if it was the last child. If a child terminates abnormally, it replaces the child with a new one and continues the loop.
- `EXIT`  is an exit signal. In Erlang, when a process terminates, it sends an exit signal to all other processes to which it is linked.
- In the `child` function, it receives `add` messages from the master. It adds the value in the message to its data, and if the new data exceeds 30, it ends naturally. It also has a chance to terminate with an error. If there is still work to do, it sends a `value` message to the master and continues.

Finally, you can run the program:
```erlang
1> exlink:start(3).
Child <0.68.0> created
Child <0.69.0> created
Child <0.70.0> created
child <0.68.0> has value 0
child <0.69.0> has value 0
child <0.70.0> has value 0
child <0.68.0> has value 5
child <0.69.0> has value 2
child <0.70.0> has value 6
child <0.68.0> has value 12
child <0.69.0> has value 12
child <0.70.0> has value 16
child <0.68.0> has value 16
... (continued)
That's all.
ok
```
This will create three children processes and print their values. The master will replace any child that terminates abnormally, and the program ends when all children have ended.

