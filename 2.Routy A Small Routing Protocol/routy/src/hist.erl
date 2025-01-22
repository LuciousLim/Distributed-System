-module(hist).

%% API
-export([new/1, update/3]).

new(Name) ->
	[{Name, inf}].

update(Node, N, History) ->
	 case lists:keyfind(Node, 1, History) of
		 {Node, M} ->
			 if
				N > M ->
					Updated = [{Node, N} | lists:keydelete(Node, 1, History)],
					{new, Updated};
				true ->
					old
	 		end;
		 false ->
			 Updated = [{Node, N} | History],
			 {new, Updated}
	end.