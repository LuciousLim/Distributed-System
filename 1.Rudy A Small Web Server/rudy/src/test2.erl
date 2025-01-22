-module(test2).
-export([bench/4]).

bench(Host, Port, M, N) ->      % M: the number of processes, N: the number of requests that each process generates
  Start = erlang:system_time(micro_seconds),
  parallel(Host, Port, M, N, self()),
  collect(M),  % synchronize
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.

parallel(Host, Port, M, N, Parent) ->
  case M of
    0 ->
      ok;
    M ->
      spawn(fun() -> loop(Host, Port, N, Parent) end),
      parallel(Host, Port, M - 1, N, Parent)
  end.

loop(Host, Port, N, Parent) ->
  run(Host, Port, N),
  Parent ! ok.

%% Each make N requests to the server
run(_, _, 0) ->
  ok;
run(Host, Port, N) ->
  request(Host, Port),
  run(Host, Port, N - 1).

%% Collects 'ok' messages from each spawned client process
collect(0) ->
  %io:format("All processes collected~n"),
  ok;
collect(K) ->
  %io:format("Waiting to receive message...~n"),
  receive
    ok ->
      %io:format("~p process left~n", [N]),
      collect(K - 1)
  end.

%% Makes a single request to the server and sends 'ok' back to the caller
request(Host, Port) ->
  Opt = [binary, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("/foo")),
  Recv = gen_tcp:recv(Server, 0),
    case Recv of
      {ok, _} ->
        ok;  %io:format("receive message, pass this to ~p~n", [Parent]);
      {error, Reason} ->
        io:format("test: error: ~w~n", [Reason])
    end,
  gen_tcp:close(Server).





% test2:bench("localhost", 12345, 10).
% test2:bench("localhost", 8080, 5, 10).

