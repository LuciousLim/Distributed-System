-module(key).

%% API
-export([generate/0, between/3]).

generate() ->
    random:uniform(1000000000).


between(Key, From, To) ->
    if
        From == To ->
            true;
        From < To ->
            (From < Key) and (Key =< To);
        From > To ->
            (From < Key) or (Key =< To)
    end.