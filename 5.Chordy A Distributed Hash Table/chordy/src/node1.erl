-module(node1).

%% API
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
	io:format("before stabilize --- node: ~p, pre: ~p, su: ~p~n", [Id, Predecessor, Successor]),
    schedule_stabilize(),
	io:format("after stabilize --- node: ~p, pre: ~p, su: ~p~n", [Id, Predecessor, Successor]),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    % if we are alone, connect to ourselves
    {ok, {Id, self()}};

connect(Id, Peer) ->
    % if we're joining a ring, send a key to the other node
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
        probe ->
			io:format("node: ~p, pre: ~p, su: ~p~n", [Id, Predecessor, Successor]),
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
			io:format("node: ~p, pre: ~p, su: ~p~n", [Id, Predecessor, Successor]),
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
			io:format("node: ~p, pre: ~p, su: ~p~n", [Id, Predecessor, Successor]),
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor)
	end.

stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			%% insert
			Spid ! {notify, {Id, self()}},
            Successor;
		{Id, _} ->
			%% point back to us, do nothing
            Successor;
		{Skey, _} ->
            %% successor point to itself, insert
            Spid ! {notify, {Id, self()}},
            Successor;
		{Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                %% The key of the predecessor of our successor (Xkey) is between us and our successor, adopt this node
				%% as our successor and run stabilization again.
                true ->
                    Xpid ! {request, self()},
                    Pred;
                % If we should be in between the nodes we inform our successor of our existence.
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
                end
	end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            % if the notified node's predecessor is set as nil then accept the new predecessor
            {Nkey, Npid};
        {Pkey, _} ->
            % if there is a predecessor then check where it stands wrt to our new notifier node
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % if notifier node is between our pred and us then add it as new pred
                    {Nkey, Npid};
                false ->
                    % if notified is not between us, return our Predecessor to it
                    Predecessor
            end
    end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).



create_probe(Id,{_,Spid}) ->
    Spid ! {probe, Id, [Id], erlang:now()}.

remove_probe(T, Nodes) ->
    Duration = timer:now_diff(erlang:now(), T),
    io:format("~nTime =~p, Ring: ~p~n", [Duration, Nodes]).

forward_probe(Ref, T, Nodes, Id, {_,Spid}) ->
    Spid ! {probe, Ref, Nodes ++ [Id], T}.