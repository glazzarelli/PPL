In Erlang, when a process calls itself (i.e., when it uses recursion to continue its execution), it does not change its Process ID (PID). The PID of an Erlang process remains constant for the lifetime of that process. This is a fundamental aspect of how processes work in Erlang.

Here's an example to illustrate this behavior:

```erlang
-module(main).
-export([start/0, loop/1]).

start() ->
    Pid = spawn(fun() -> loop(0) end),
    io:format("Started process with PID: ~p~n", [Pid]).

loop(Count) ->
    io:format("Loop iteration ~p, PID: ~p~n", [Count, self()]),
    receive
        stop ->
            io:format("Stopping process with PID: ~p~n", [self()])
    after 1000 ->
        loop(Count + 1)
    end.
```

In this example, the `start/0` function spawns a new process running the `loop/1` function. The `loop/1` function recursively calls itself, incrementing the `Count` on each iteration. Each time it prints the iteration count and the PID of the process.

You can compile and run this module in the Erlang shell:
```erlang
1> c(main).
{ok,main}
2> main:start().
Started process with PID: <0.87.0>
Loop iteration 0, PID: <0.87.0>
ok
Loop iteration 1, PID: <0.87.0>
Loop iteration 2, PID: <0.87.0>
Loop iteration 3, PID: <0.87.0>
Loop iteration 4, PID: <0.87.0>
Loop iteration 5, PID: <0.87.0>
```

