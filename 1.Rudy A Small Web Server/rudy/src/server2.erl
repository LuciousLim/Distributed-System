-module(server2).
-author("lenovo").

%% API
-export([start/2, stop/0]).

%%% ===============================================================
%%% API functions
%%% ===============================================================
start(Port, N) ->
    register(rudy, spawn(fun() -> init(Port, N) end)).

stop() ->
    exit(whereis(rudy), "time to die").

%% ===============================================================
%% Internal functions
%% ===============================================================
init(Port, N) ->
    Opt = [binary, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            spawn_handlers(Listen, N),
            control();
        {error, Error} ->
            {error, Error}
    end.

control() ->
    receive
        stop ->
            ok
    end.

%% Spawns multiple acceptor processes
spawn_handlers(Listen, 0) ->
    ok;
spawn_handlers(Listen, N) ->
    spawn(fun() -> handler(Listen) end),
    spawn_handlers(Listen, N - 1).

%% Accepts connections and spawns a handler for each connection
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);  % Keep accepting more clients
        {error, Error} ->
            io:format("Error in acceptor: ~w~n", [Error])
%%            timer:sleep(1000), % Retry after some delay
%%            acceptor(Listen)
    end,
    control().

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            %io:format("rudy: parsing request~n", []),
            Request = http:parse_request(binary_to_list(Str)),
            %io:format("rudy: sending reply~n", []),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
            %io:format("~p response success~n", [self()]);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).


%%reply({{get, URI, _}, _, _}) ->
%%    %timer:sleep(40),
%%    http:ok("<html>
%%                <body>
%%                    Test Server : " ++ URI ++ "
%%                </body>
%%            </html>").

reply({{get, URI, _}, _, _}) ->
	[$/|FileName] = URI,
	case file:read_file(FileName) of
		{ok, Data} ->
      %timer:sleep(10),
			http:ok([Data]);
		{error, Reason} ->
			io:format("Error case file: ~s~n", [FileName]),
			Msg = "Get request received for non-existing file: " ++ FileName,
 			http:ok([Msg])
	end.
