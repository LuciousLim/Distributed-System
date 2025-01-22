-module(vtime).

%% API
-export([zero/1, inc/2, merge/2, leq/2, clock/1, update/3,safe/2]).

zero(Nodes) ->
	TempList = [],
	lists:foldl(fun(Node, Acc) -> [{Node, 0} | Acc] end, TempList, Nodes).

inc(Name, Time) ->
	case lists:keyfind(Name, 1, Time) of
		{_, Counter} ->
			lists:keyreplace(Name, 1, Time, {Name, Counter + 1});
		false ->
			[{Name, 1}|Time]
	end.

merge([], Time) ->
	Time;
merge([{Name, Ti}|Rest], Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
			if
				Ti > Tj -> T = Ti;
				true ->  T = Tj
			end,
			[{Name, T} | merge(Rest, lists:keydelete(Name, 1, Time))];
		false ->
			[{Name, Ti} | merge(Rest, Time)]
	end.

leq([], _) ->
	true;
leq([{Name, Ti}|Rest],Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
			if
				Ti < Tj ->
					leq(Rest, Time);
				true ->
					false
			end;
		false ->
			false
	end.

clock(_) ->
	[].

update(From, Time, Clock) ->
	{Name, Ti} = lists:keyfind(From, 1, Time),
	case lists:keyfind(From, 1, Clock) of
		{Node, _} ->
			lists:keyreplace(Node, 1, Clock, {Name, Ti});
		false ->
			[{Name, Ti}| Clock]
	end.

safe(Time, Clock) ->
	leq(Time, Clock).