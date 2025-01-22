-module(dijkstra).

%% API
-export([table/2, route/2, iterate/3]).

%% *******************************************************************
%% API Function
%% *******************************************************************

table(Gateway,Map) ->            %% Sort the nodes by their initial distances, finding the shortest paths to all nodes.
    Sorted = initialize(Gateway, Map),
    iterate(Sorted, Map, []).

route(Node, Table) ->           %% Search the routing table and return the gateway suitable to route messages to a node.
    case lists:keyfind(Node, 1, Table) of
        false ->
            notfound;
        {Node, unkonwn} ->
            notfound;
        {Node, Gateway} ->
            {ok, Gateway}
    end.


%% *******************************************************************
%% Internal Function
%% *******************************************************************

initialize(Gateway, Map) ->
    Nodes = map:all_nodes(Map),
    Non_GW = lists:subtract(Nodes, Gateway),
    Init_GW = lists:map(fun (Nd) -> {Nd,0,Nd} end, Gateway),
    Init_Non_GW = lists:map(fun (Nd) -> {Nd,inf,unknown} end, Non_GW),
    lists:sort(fun({_, N0, _}, {_, N1, _}) -> N0 < N1 end, Init_Non_GW ++ Init_GW).

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {_, Length, _} -> Length;
        false -> 0            %% Node is not found
    end.

replace(Node, N, Gateway, Sorted) ->
    DeletedList = lists:keydelete(Node, 1, Sorted),       %% Delete old entry
    L = [{Node, N, Gateway} | DeletedList],       %% Add new entry
    lists:sort(fun({_, N0, _}, {_, N1, _}) -> N0 < N1 end, L).

update(Node, N, Gateway, Sorted) ->
    Old_len = entry(Node, Sorted),
    case N < Old_len of
        true ->
            replace(Node, N, Gateway, Sorted);
        false ->
            Sorted
    end.

iterate(Sorted, Map, Table) ->
    case Sorted of
        [] ->               %% All nodes have been processed
            Table;
        [{_,inf,_}|_] ->
            Table;
        [{CurrentNode, N, OldGateway} | RestSorted] ->
            if N == inf ->
                Table;
            true ->           %%Take the first node from Sorted, find its neighbors from Map, and check if there’s a shorter path through this node
                Neighbors = get_neighbors(CurrentNode, Map),
                UpdatedSorted = lists:foldl(fun(Node, Acc) -> update(Node, N + 1, OldGateway, Acc) end, RestSorted, Neighbors),
                iterate(UpdatedSorted, Map, [{CurrentNode, OldGateway} | Table])
            end
    end.


get_neighbors(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, Neighbors} -> Neighbors;
        false -> []
    end.


%%process_neighbors(Neighbors, Node, N, Gateway, Sorted, Table, Map) ->
%%  case Neighbors of
%%    [] ->                                     %% If no neighbors to process, return unchanged
%%      {Sorted, Table};
%%    [Neighbor | RestNeighbors] ->
%%%%      io:format("~p~n", [Neighbor | RestNeighbors]),    %% *******************************
%%      NewDist = entry(Node, Sorted) + N,
%%%%      io:format("~p~n", [NewDist]),                     %% *******************************
%%%%      io:format("~p~n", [entry(Neighbor, Sorted)]),                     %% *******************************
%%      case entry(Neighbor, Sorted) of
%%        inf ->                                %% If Neighbor not in the list, add it
%%          UpdatedSorted = update(Neighbor, NewDist, Node, Sorted),
%%          UpdatedTable = [{Neighbor, Node} | Table],
%%          process_neighbors(RestNeighbors, Node, N, Gateway, UpdatedSorted, UpdatedTable, Map);
%%        OldDiist ->
%%          if NewDist < OldDiist ->            %% Found a shorter path, update list and table
%%            UpdatedSorted = update(Neighbor, NewDist, Node, Sorted),
%%            UpdatedTable = lists:keyreplace(Neighbor, 1, Table, {Neighbor, Node}),
%%            process_neighbors(RestNeighbors, Node, N, Gateway, UpdatedSorted, UpdatedTable, Map);
%%          true ->                             %% No shorter path, continue with the next neighbor
%%            process_neighbors(RestNeighbors, Node, N, Gateway, Sorted, Table, Map)
%%          end
%%      end
%%  end.
