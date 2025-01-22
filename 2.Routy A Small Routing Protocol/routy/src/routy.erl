-module(routy).

%% API
-export([start/2, stop/1, router/6, displayStatus/1]).

start(Reg, Name) ->
	register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
	Node ! stop,
	unregister(Node).

init(Name) ->
	Intf = interface:new(),
	Map = map:new(),
	Table = dijkstra:table(Intf, Map),
	Hist = hist:new(Name),
	router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
	receive
		{add, Node, Pid} ->
			Ref = erlang:monitor(process,Pid),
			Intf1 = interface:add(Node, Ref, Pid, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
			%% io:format("receive");

		{remove, Node} ->
			{ok, Ref} = interface:ref(Node, Intf),
			erlang:demonitor(Ref),
			Intf1 = intf:remove(Node, Intf),
			router(Name, N, Hist, Intf1, Table, Map);

		{'DOWN', Ref, process, _, _} ->
			{ok, Down} = interface:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			Intf1 = interface:remove(Down, Intf),
			router(Name, N, Hist, Intf1, Table, Map);

		{status, From} ->
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			router(Name, N, Hist, Intf, Table, Map);

		{links, Node, R, Links} ->
			case hist:update(Node, R, Hist) of
				{new, Hist1} ->
					interface:broadcast({links, Node, R, Links}, Intf),
					UpdatedMap = map:update(Node, Links, Map),
					router(Name, N, Hist1, Intf, Table, UpdatedMap);
				old ->
					router(Name, N, Hist, Intf, Table, Map)
			end;

		update ->
			Table1 = dijkstra:table([Name | interface:list(Intf)], Map),
			%%Table1 = dijkstra:table(interface:list(Intf), Map),
			router(Name, N, Hist, Intf, Table1, Map);

		broadcast ->
			Message = {links, Name, N, interface:list(Intf)},
			interface:broadcast(Message, Intf),
			router(Name, N+1, Hist, Intf, Table, Map);

		{route, Name, From, Message} ->						%% Message has arrived at the final destination
			io:format("(~w): received message (~w) from (~p) ~n", [Name, Message, From]),
			router(Name, N, Hist, Intf, Table, Map);

		{route, To, From, Message} ->						%% This is not the destination of the message.
			io:format("(~w): routing message (~p) from (~w) to (~p)~n", [Name, Message, From, To]),
			case dijkstra:route(To, Table) of
				{ok, Gw} ->
					case interface:lookup(Gw, Intf) of
						{ok, Pid} ->
							Pid ! {route, To, From, Message};
						notfound ->
							ok
					end;
				notfound ->
					ok
			end,
			router(Name, N, Hist, Intf, Table, Map);

		{send, To, Message} ->						%% For local user to initiate the routing of a message
			self() ! {route, To, Name, Message},
			router(Name, N, Hist, Intf, Table, Map);

		stop ->
			ok
	end.

displayStatus(Router) ->
    Router ! {status, self()},
    receive
		{status, {Name, N, Hist, Intf, Table, Map}} ->
			io:format("Status -------------~n"),
			io:format(" name: ~w~n", [Name]),
			io:format("    n: ~w~n", [N]),
			io:format(" hist: ~w~n", [Hist]),
			io:format(" intf: ~w~n", [Intf]),
			io:format("table: ~w~n", [Table]),
			io:format("  map: ~w~n", [Map]),
			ok;
		true ->
			io:format("  got something: ~n")

	end.