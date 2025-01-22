-module(vloggy).

%% API
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	Queue = [],
	Clock = vtime:clock(Nodes),
	loop(Queue, Clock).

loop(Queue, Clock) ->
	receive
		{log, From, Time, Msg} ->
			NewQueue = lists:keysort(2, [{From, Time, Msg} | Queue]),
			UpdatedClock = vtime:update(From, Time, Clock),
			UpdatedQueue = checkSafe(NewQueue, UpdatedClock),
			loop(UpdatedQueue, UpdatedClock);
		stop ->
			ok
	end.

log(From, Time, Msg) ->
	io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

checkSafe(Queue, Clock) ->
	case Queue of
		[] ->
			[];
		Queue ->
			[{From, Time, Msg} | Rest] = Queue,
			case vtime:safe(Time, Clock) of
				true ->
					log(From, Time, Msg),
					checkSafe(Rest, Clock);
				false ->
					Queue
			end
	end.
