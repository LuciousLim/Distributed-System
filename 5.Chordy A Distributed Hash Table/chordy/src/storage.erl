
-module(storage).

-export([create/0,add/3,lookup/2,merge/2,split/3]).

%% create a new store
create() ->
  	[].

%% add a key value pair, return the updated store
add(Key, Value, Store) ->
   	[{Key, Value} | Store].

%% return a tuple fKey, Valueg or the atom false
lookup(Key, Store)->
  	lists:keyfind(Key, 1, Store).

%% add a list of key-value pairs to a store
merge(Entries, Store)->
  	lists:merge(Entries, Store).

%% return a tuple {Updated, Rest} where the updated store only contains the key-value pairs requested
%% and the rest are found in a list of key-value pairs;
split(From, To, Store) ->
  	lists:partition(fun({Key,_}) -> key:between(Key, From, To) end, Store).
