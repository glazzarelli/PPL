### Parallel Map
Write a function `pmap/2` that applies a given function to each element of a list in parallel.

Example:
```erlang
pmap(fun (X) -> X*X end, [1,2,3,4])
% [1,4,9,16]
```

Solution:
```erlang
pmap(F, L) ->
    Ps = [spawn(?MODULE, execute, [F, X, self()]) || X <- L],
    [receive 
         {Pid, X} -> X
     end || Pid <- Ps].

execute(F, X, Pid) ->
	Pid ! {self(), F(X)}.
```

>[!help]- Explanation
>The `pmap` function takes two arguments: `F` (a function) and `L` (a list). It then creates a new process for each element in `L` by calling the `spawn` function with three arguments: the current module (`?MODULE`), the `execute` function, and a list containing `F`, an element `X` from `L`, and the process ID of the current process (`self()`). This results in a list `Ps` of process IDs.
> 
> Then it uses a list comprehension to receive a message from each process in `Ps`. Each message should be a tuple containing the process ID and the result of applying `F` to the corresponding element of `L`. The received value `X` is then returned.
> 
> The `execute` function is called by each spawned process. It takes three arguments: `F` (a function), `X` (an element from `L`), and `Pid` (the process ID of the parent process). It applies `F` to `X` and sends a message to the parent process with its own process ID and the result of `F(X)`.
> 
> In summary, the `pmap` function parallels the execution of a function over a list in Erlang. Each element of the list is processed in a separate process, and the results are collected and returned as a list.


### Parallel Filter

Write a function `pfilter/2` that filters a list based on a given predicate in parallel.

Example:
```erlang
pfilter(fun (X) -> X > 2 end, [1,2,3,4])
% [3,4]
```

Solution:
```erlang
pfilter(P, L) ->
  Ps = [spawn(?MODULE, check, [P, X, self()]) || X <- L],
  lists:foldl(fun (Pid, Vo) ->
                      receive
                          {Pid, true, X} -> Vo ++ [X];
                          {Pid, false, _} -> Vo
                      end
             end, [], Ps).

check(P, X, Pid) ->
  Pid ! {self(), P(X), X}.
```

>[!help]- Explanation
>The function `pfilter(P, L)` takes two arguments, a predicate function `P` and a list `L`.
> 
> 1. It uses list comprehension to spawn a new process for each element `X` in the list `L`, where each process executes the `check` function with arguments `P`, `X`, and `self()`. The `self()` function returns the process ID of the current process, i.e., the process that calls `self()`. The list of spawned processes is stored in `Ps`.
> 
> 2. The `check` function sends a message to the process `Pid` (which is the process that spawned this process) containing a tuple `{self(), P(X), X}`, where `self()` is the process ID of the current process (i.e., the process that runs the `check` function), `P(X)` is the result of applying the predicate function `P` to `X`, and `X` itself.
> 
> 3. The `pfilter` function then folds over the list of spawned processes `Ps`. For each process `Pid` in `Ps`, it waits to receive a message from `Pid`. If the message indicates that `P(X)` is true (i.e., `{Pid, true, X}`), it appends `X` to the result list `Vo`. If `P(X)` is false, it leaves the result list unchanged. The initial result list is empty.


### Partition List

Write a function `partition/2` that partitions a list into N chunks.

Example:
```erlang
partition([1,2,3,4,5,6,7,8,9,10], 3)
% [[1,2,3],[4,5,6],[7,8,9,10]]
```

Solution:
```erlang
partition(L, N) -> 
    M = length(L),
    Chunk = M div N,
    End = M - Chunk*(N-1),
    parthelp(L, N, 1, Chunk, End, []).

parthelp(L, 1, P, _, E, Res) ->
    Res ++ [lists:sublist(L, P, E)];
parthelp(L, N, P, C, E, Res) ->
    R = lists:sublist(L, P, C),
    parthelp(L, N-1, P+C, C, E, Res ++ [R]).
```

>[!help]- Explanation
>The function `partition/2` first calculates the total length of the list `M` and then calculates the size of each chunk `Chunk` (except for the last one which might be slightly larger) by integer division of `M` by `N`. The variable `End` is the size of the last chunk, calculated by subtracting the total size of the `N-1` first chunks from `M`.
> 
> The function `parthelp/6` is a recursive helper function that constructs the chunks. It takes six arguments: `L` is the original list, `N` is the number of remaining chunks to construct, `P` is the position in the list where the next chunk starts, `C` is the size of the next chunk, `E` is the size of the last chunk, and `Res` is the list of chunks constructed so far.
> 
> If `N` is 1, it means the last chunk is being constructed, so it appends a chunk of size `E` starting from position `P` in `L` to `Res`.
> 
> If `N` is greater than 1, it means there are more chunks to construct. It constructs the next chunk `R` of size `C` starting from position `P` in `L`, appends it to `Res`, and then recursively calls itself with `N` decremented by 1 and `P` incremented by `C`.
> 
> The function `lists:sublist/3` is a built-in Erlang function which extracts a sublist of a given length from a given starting position in a list.

### Parallel Fold

Write a function `parfold/3` that folds a list in parallel using a given function.

Example:
```erlang
parfold(fun (X, Y) -> X + Y end, [1,2,3,4,5,6,7,8,9,10], 3)
% 55
```

Solution:
```erlang
%% From previous exercise %%
partition(L, N) -> 
    M = length(L),
    Chunk = M div N,
    End = M - Chunk*(N-1),
    parthelp(L, N, 1, Chunk, End, []).

parthelp(L, 1, P, _, E, Res) ->
    Res ++ [lists:sublist(L, P, E)];
parthelp(L, N, P, C, E, Res) ->
    R = lists:sublist(L, P, C),
    parthelp(L, N-1, P+C, C, E, Res ++ [R]).
%% % %%

parfold(F, L, N) ->
    Ls = partition(L, N),
    Ps = [spawn(?MODULE, dofold, [self(), F, X]) || X <- Ls],
    [R|Rs] = [receive
                  {P, V} -> V 
              end || P <- Ps],
    lists:foldl(F, R, Rs).

dofold(Pid, F, [X|Xs]) ->
    Pid ! {self(), lists:foldl(F, X, Xs)}.
```

>[!help]- Explanation
>The Erlang `parfold/3` function is a parallel version of the `lists:foldl/3` function. It partitions the list `L` into `N` chunks, processes each chunk in a separate Erlang process, and then combines the results.
> 
> Here is the breakdown of the code:
> 
> `parfold(F, L, N)` function:
> - `Ls = partition(L, N)` partitions the list `L` into `N` chunks.
> - `Ps = [spawn(?MODULE, dofold, [self(), F, X]) || X <- Ls]` spawns `N` processes, each of which applies the function `F` to one of the chunks and sends the result back to the spawning process.
> - The spawned processes use `dofold/3` function to process each chunk.
> `dofold(Pid, F, [X|Xs])` function:
> - `Pid ! {self(), lists:foldl(F, X, Xs)}` applies the function `F` to the chunk `[X|Xs]` and sends the result back to `Pid`.
> Back in `parfold/3`, the function collects the results from the spawned processes, combines them using `F`, and returns the final result:
> - `[R|Rs] = [receive {P, V} -> V end || P <- Ps]` receives the results from the spawned processes.
> - `lists:foldl(F, R, Rs)` combines the results using `F`.
> 
> This function allows you to use all available CPU cores for computation by running the function `F` in parallel on different chunks of the list.


### Process Linking and Monitoring

Write a function `master/1` that spawns a list of functions as linked processes and monitors their execution.

Example:
```erlang
master([fun () -> timer:sleep(1000), io:fwrite("Hello, World!\n") end])
% "Hello, World!\n"
% ok
```

Solution:
```erlang
listlink([], Pids) -> Pids;
listlink([F|Fs], Pids) ->
    Pid = spawn_link(F),
    listlink(Fs, Pids#{Pid => F}).

master(Functions) ->
    process_flag(trap_exit, true),
    Workers = listlink(Functions, #{}),
    master_loop(Workers, length(Functions)).

master_loop(_, 0) ->
    ok; 
master_loop(Workers, Count) ->
    receive
        {'EXIT', _, normal} ->
            master_loop(Workers, Count-1);
        {'EXIT', Pid, _} ->
            #{Pid := Fun} = Workers,
            Child = spawn_link(Fun),
            master_loop(Workers#{Child => Fun}, Count)
    end.
```