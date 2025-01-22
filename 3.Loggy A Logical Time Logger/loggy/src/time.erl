-module(time).

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).


zero() ->
	0.

inc(Name, T) ->
	T + 1.

merge(Ti, Tj) ->
	if
		Ti =< Tj -> Tj;
		true -> Ti
	end.

leq(Ti,Tj) ->
	if
		Ti =< Tj -> true;
		true -> false
	end.

%% Return a clock that can keep track of the nodes
clock(Nodes) ->
	TempList = [],
	NodeList = lists:foldl(fun(Node, Acc) -> [{Node, 0} | Acc] end, TempList, Nodes),
	lists:keysort(2, NodeList).

%% Return a clock that has been updated given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
	case lists:keyfind(Node, 1, Clock) of
		false ->
			lists:keysort(2, [{Node, Time} | Clock]);
		{_, _} ->
			lists:keysort(2, lists:keyreplace(Node, 1, Clock, {Node, Time}))
	end.



%% Judge whether it is safe to log an event that happened at a given time
safe(Time, Clock) ->
	[{_, Min} | _] = Clock,
	leq(Time, Min).


