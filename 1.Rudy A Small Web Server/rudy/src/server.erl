-module(server).
-export([start/1, stop/0, init/1]).

%% ===============================================================
%% API functions
%% ===============================================================
start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die"),
    exit(whereis(p1), "time to die"),
    exit(whereis(p2), "time to die").


%% ===============================================================
%% Internal functions
%% ===============================================================
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            register(p1, spawn(fun() -> handler(Listen) end)),
            register(p2, spawn(fun() -> handler(Listen) end)),
            %%handler(Listen),
            control();
        {error, Error} ->
            error
    end.

control() ->
    receive
        stop ->
            ok
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
            end,
    gen_tcp:close(Client).

%%reply({{get, URI, _}, _, _}) ->
%%    %timer:sleep(40),
%%    %http:ok("a reply from the server").
%%    http:ok("<html>
%%				<body>
%%					Test Server : " ++ URI ++ "
%%				</body>
%%				</html>").

reply({{get, URI, _}, _, _}) ->
	[$/|FileName] = URI,
	case file:read_file(FileName) of
		{ok, Data} ->
			http:ok([Data]);
		{error, Reason} ->
			io:format("Error case file: ~s~n", [FileName]),
			Msg = "Get request received for non-existing file: " ++ FileName,
 			http:ok([Msg])
	end.