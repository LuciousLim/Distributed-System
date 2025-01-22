-module(node3).

%% API
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

stop() ->
	exit(self()).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, nil, []).

connect(Id, nil) ->
    % if we are alone, connect to ourselves
	Ref = monitor(self()),
    {ok, {Id, Ref, self()}};

connect(Id, Peer) ->
    % if we're joining a ring, send a key to the other node
    Qref = make_ref(),
	Ref = monitor(Peer),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Ref, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.







node(Id, Predecessor, Successor, Next, Store) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Next, Store);
		{notify, New} ->
			{Pred, NewStore} = notify(New, Id, Predecessor, Store),
			node(Id, Pred, Successor, Next, NewStore);
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor, Next, Store);
		{status, Pred} ->
			{Succ, Nx} = stabilize(Pred, Id, Successor, Next),
			node(Id, Predecessor, Succ, Next, Store);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Next, Store);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);
		{add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
	        node(Id, Predecessor, Successor, Next, Added);
	    {lookup, Key, Qref, Client} ->
	        lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
	        node(Id, Predecessor, Successor, Next, Store);
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Next, Merged);
		{'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nx} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Nx, Store);
		stop ->
            stop()
	end.

stabilize(Pred, Id, Successor, Next) ->
	{Skey, Sref, Spid} = Successor,
	case Pred of
		nil ->
			%% insert
			Spid ! {notify, {Id, self()}},
			{Successor, Next};
		{Id, _, _} ->
			%% point back to us, do nothing
            {Successor, Next};
		{Skey, _, _} ->
            %% successor point to itself, insert
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
		{Xkey, _, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                %% The key of the predecessor of our successor (Xkey) is between us and our successor, adopt this node
				%% as our successor and run stabilization again.
                true ->
					Xref = monitor(Xpid),
                    drop(Sref),
                    self() ! stabilize,
                    {{Xkey, Xref, Xpid}, Successor};
                % If we should be in between the nodes we inform our successor of our existence.
                false ->
					Spid ! {notify, {Id, self()}},
            		{Successor, Next}
                end
	end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            % if the notified node's predecessor is set as nil then accept the new predecessor
			Keep = handover(Store, Id, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            % if there is a predecessor then check where it stands wrt to our new notifier node
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % if notifier node is between our pred and us then add it as new pred
					Keep = handover(Store, Id, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    % if notified is not between us, return our Predecessor to it
                    Predecessor
            end
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  	case key:between(Key, Pkey, Id) of
    	true ->
      		Client ! {Qref, ok},
      		storage:add(Key, Value, Store);
		false ->
      		Spid ! {add, Key, Value, Qref, Client},
      		Store
  	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store)->
  	case key:between(Key, Pkey, Id) of
    	true ->
      		Result = storage:lookup(Key, Store),
      		Client ! {Qref, Result};
    	false ->
      		{_, Spid} = Successor,
      		Spid ! {lookup, Key, Qref, Client}
  	end.

handover(Store, Id, Nkey, Npid) ->
	{Rest, Keep} = storage:split(Nkey, Id, Store),
	Npid ! {handover, Rest},
	Keep.

create_probe(Id,{_,Spid}) ->
    Spid ! {probe, Id, [Id], erlang:now()}.

remove_probe(T, Nodes) ->
    Duration = timer:now_diff(erlang:now(), T),
    io:format("~n Time =~p --- Ring: ~p~n", [Duration, Nodes]).

forward_probe(Ref, T, Nodes, Id, {_,Spid}) ->
    Spid ! {probe, Ref, Nodes ++ [Id], T}.

monitor(Pid) ->
	erlang:monitor(process, Pid).

drop(nil) ->
	ok;

drop(Pid) ->
	erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Nx) ->
	{nil, Successor, Nx};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
	self() ! stabilize,
 	Nref = monitor(Npid),
	{Predecessor, {Nkey, Nref, Npid}, nil}.