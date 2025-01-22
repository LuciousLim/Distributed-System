-module(http).
-export([parse_request/1, ok/1, get/1]).

%% ===============================================================
% The constitution of a request:
% Request = Request-Line;       
% *(( general-header            
% | request-header ;                
% | entity-header ) CRLF) ; 
% CRLF
% [ message-body ] ; 
%% ===============================================================


%% ===============================================================
%% Request parser
%% ===============================================================
parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

%% ===============================================================
%% Request line parser
%% ===============================================================
request_line([$G, $E, $T, 32 |R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
%%    [13,10|R3] = R2,
%%    {{get, URI, Ver}, R3}.
    case R2 of
        [13, 10 | R3] -> {{get, URI, Ver}, R3};
        _ -> {error, R0}  % If the CRLF is not present, return error
    end.

%% ===============================================================
%% URI parser
%% ===============================================================
request_uri([32|R0])->                      
    {[], R0};
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C|Rest], R1}.


%% ===============================================================
%% Version parser (simple version, only works between 1.0 and 1.1)
%% ===============================================================
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.


%% ===============================================================
%% Headers parser
%% ===============================================================
%parse sigle header
header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.

%parse list of headers
headers([13,10|R0]) ->                    
    {[], R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.


%% ===============================================================
%% Headers parser
%% ===============================================================
message_body(R) ->
{R, []}.

%% ===============================================================
%% Reply generator
%% ===============================================================
ok(Body) ->
"HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

%% ===============================================================
%% URI generator
%% ===============================================================
get(URI) ->
"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
