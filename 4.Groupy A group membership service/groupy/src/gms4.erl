-module(gms4).
-export([start/1, start/2]).
-define(timeout, 1000).
-define(timeout2, 10000).
-define(arghh, 200).
-define(recv, 100).

start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 0, [], [Master], 0).

start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, N, [Leader|Slaves], Group} ->
			erlang:monitor(process, Leader),
			Master ! {view, Group},
			case recv() of
				true ->
					timer:sleep(10),
					Leader ! {ack, {view, N, [Leader|Slaves], Group}, self()},
					slave(Id, Master, Leader, N + 1, {view, N, [Leader|Slaves], Group} ,Slaves, Group);
				false ->
					init(Id, Rnd, Grp, Master),
					io:format("message {view} not recv~n")
			end
	after
		?timeout2 ->
		Master ! {error, "no reply from leader"}
	end.

leader(Id, Master, N, Slaves, Group, K) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, N, Msg}, Slaves),
			Master ! Msg,
			case length(Slaves) of
				0 ->
					leader(Id, Master, N + 1, Slaves, Group, length(Slaves));
				_ ->
%%					io:format("1~n"),
					resend(Id, Slaves, K, {msg, N, Msg}),
					timer:sleep(100),
%%					io:format("2~n"),
					leader(Id, Master, N + 1, Slaves, Group, length(Slaves))
			end;
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
%%			io:format("3~n"),
			resend(Id, Slaves2, K + 1, {view, N, [self()|Slaves2], Group2}),
			timer:sleep(100),
%%			io:format("4~n"),
			Master ! {view, Group2},
			leader(Id, Master, N + 1, Slaves2, Group2, length(Slaves2));
		stop ->
			ok
	end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, N, Last, Slaves, Group);

		{msg, I, _} when I < N->
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, N, Msg} ->
			case recv() of
				true ->
					timer:sleep(10),
					Leader ! {ack, {msg, N, Msg}, self()},
					Master ! Msg,
%%					io:format("msg: ~p~n", [{ack, {msg, N, Msg}, self()}]),
					slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group);
				false ->
					io:format("message {msg} not recv~n"),
					slave(Id, Master, Leader, N, Last, Slaves, Group)
			end;

%%		{view, I, _, _} when I < N->
%%			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{view, N, [Leader|Slaves2], Group2} ->
			case recv() of
				true ->
					timer:sleep(10),
					Leader ! {ack, {view, N, [Leader|Slaves2], Group2}, self()},
%%					io:format("view: ~p~n", [{ack, {view, N, [Leader|Slaves2], Group2}, self()}]),
					Master ! {view, Group2},
					slave(Id, Master, Leader, N + 1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
				false ->
					io:format("message {view} not recv~n"),
					slave(Id, Master, Leader, N, Last, Slaves, Group)
			end;


		{'DOWN', _Ref, process, Leader, _Reason} ->
			%% io:format("slave ~p detects ~p down, reason: ~p~n", [Id, Leader, _Reason]),
			election(Id, Master, N, Last, Slaves, Group);

		stop ->
			ok
	end.

resend(Id, Slaves, K, Msg) ->
%%	io:format("K1:~p~n", [K]),
	case ack(Msg, K, Slaves) of
		[] ->
			ok;
		Lost ->
			bcast(Id, Msg, Lost),
			io:format("resend msg~n"),
			resend(Id, Lost, length(Lost), Msg)
	end.

ack(Msg, K, Lost) ->
%%	io:format("ack start, K:~p~n", [K]),
	case K of
		0 ->
%%			io:format("K == 0~n"),
%%			io:format("Lost: ~p~n", [Lost]),
			Lost;
		K ->
%%			io:format("K == ~p~n", [K]),
			receive
				{ack, Msg, Pid} ->
%%					io:format("ok: ~p~n", [K]),
					ack(Msg, K - 1, lists:filter(fun(X) -> X =/= Pid end, Lost))
			after
				?timeout ->
%%					io:format("Msg: ~p timeout~n", [Msg]),
%%					io:format("~p timeout~n", [K]),
					ack(Msg, K - 1, Lost)
			end
	end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  	Self = self(),
  	case Slaves of
    	[Self|Rest] ->
			timer:sleep(10),		%% wait for all the slaves to become aware of the crash of the leader
%%      		io:format("Leader : ~w~n",[Id]),
      		bcast(Id, Last, Rest),
%%			resend(Id, Rest, length(Rest), Last),
      		bcast(Id, {view, Slaves, Group}, Rest),
			resend(Id, Rest, length(Rest), {view, N, Slaves, Group}),
      		Master ! {view, Group},
      		leader(Id, Master, N + 1, Rest, Group, length(Rest));
    	[Leader|Rest] ->
%%      		io:format("Slave : ~w~n",[Id]),
      		erlang:monitor(process, Leader),
      		slave(Id, Master, Leader, N, Last, Rest, Group)
  end.

bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
	case random:uniform(?arghh) of
		?arghh ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck),
			ok;
		_ ->
			ok
	end.

recv() ->
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	random:seed(MegaSecs, Secs, MicroSecs),
	case random:uniform(?recv) of
		?recv ->
			false;
		_ ->
			true
	end.


