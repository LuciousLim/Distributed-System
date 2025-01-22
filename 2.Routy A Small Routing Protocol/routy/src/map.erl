-module(map).

%% API
-export([all_nodes/1, new/0, update/3, reachable/2]).

new() ->
    [].

update(Node, Links, Map) ->
    %% Remove old entry
    DeletedMap = lists:keydelete(Node, 1, Map),
    %% Add new entry
    [{Node, Links} | DeletedMap].

reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        false -> [];
        {Node, Links} -> Links
    end.

all_nodes(Map) ->
    First_pos = [Node || {Node, _} <- Map],               %% Extract the node from the first position
    Second_pos = lists:flatten([Links || {_, Links} <- Map]),         %% Extract the node from the second position
    lists:usort(First_pos ++ Second_pos).             %% Conmbine two lists and deal with the duplicates
