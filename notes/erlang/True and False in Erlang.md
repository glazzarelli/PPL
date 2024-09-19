```erlang
-module(main).
-export([test/1]).

test(X) ->
    if
        X -> io:format("X is considered true~n");
        true -> io:format("X is considered false~n")
    end.
```

```erlang
1> c(main).
{ok,main}
2> main:test(true).
X is considered true
ok
3> main:test(false).
X is considered false
ok
4> main:test(0).   
X is considered false
ok
5> main:test([]).  
X is considered false
ok
6> main:test(42).  
X is considered false
ok
7> main:test("Hello").
X is considered false
ok
8> main:test([1]). 
X is considered false
ok
```