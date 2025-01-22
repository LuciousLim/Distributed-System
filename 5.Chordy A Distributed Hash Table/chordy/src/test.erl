-module(test).

-compile(export_all).

-define(Timeout, 1000).


start(Module) ->
    Id = key:generate(),
    apply(Module, start, [Id]).

start(Module, P) ->
    Id = key:generate(),
    apply(Module, start, [Id,P]).

start_test_add(NumTests, NumElements, Node) ->
    lists:foreach(fun(_) -> spawn(fun() -> run_test_add(NumElements, Node) end) end, lists:seq(1, NumTests)).

start_test_check(NumTests, NumElements, Node) ->
    lists:foreach(fun(_) -> spawn(fun() -> check(NumElements, Node) end) end, lists:seq(1, NumTests)).

start_test_processes(NumTests, NumElements, Node) ->
    lists:foreach(fun(_) -> spawn(fun() -> run_test(NumElements, Node) end) end, lists:seq(1, NumTests)).

run_test_add(NumElements, Node) ->
    Keys = test:keys(NumElements),
    add(Keys, Node).

run_test(NumElements, Node) ->
    Keys = test:keys(NumElements),
    test:add(Keys, Node),
    test:check(Keys, Node).

test1(Module) ->
    Node = start(Module),
    %% start 4 test processes, sending mesgs to 1 node
    start_test_add(4, 1000, Node).

start(_, 0, _) ->
    ok;
start(Module, N, P) ->
    start(Module, P),
    start(Module, N-1, P).

add(Key, Value , P) ->
    Q = make_ref(),
    P ! {add, Key, Value, Q, self()},
    receive
	{Q, ok} ->
	   ok
	after ?Timeout ->
	    {error, "timeout"}
    end.

lookup(Key, Node) ->
    Qref = make_ref(),
    Node ! {lookup, Key, Qref, self()},
    receive
	{Q, Value, Id} ->
		{Value, Id}
    after ?Timeout ->
	    {error, "timeout"}
    end.


keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1,N)).

add(Keys, P) ->
    lists:foreach(fun(K) -> add(K, hello, P) end, Keys).

check(Keys, P) ->
    T1 = now(),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
    io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).


check([], _, Failed, Timeout) ->
    {Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
    case lookup(Key,P) of
		{{Key, _}, Id} ->
	    	check(Keys, P, Failed, Timeout);
		{error, _} ->
	    	check(Keys, P, Failed, Timeout+1);
		false ->
	    	check(Keys, P, Failed+1, Timeout)
    end.











