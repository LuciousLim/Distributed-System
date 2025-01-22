-module(test).
-export([run/2]).

run(Sleep, Jitter) ->
	Log = loggy:start([a, b, c, d]),
	A = worker:start(a, Log, 13, Sleep, Jitter),
	B = worker:start(b, Log, 23, Sleep, Jitter),
	C = worker:start(c, Log, 36, Sleep, Jitter),
	D = worker:start(d, Log, 49, Sleep, Jitter),
	worker:peers(A, [B, C, D]),
	worker:peers(B, [A, C, D]),
	worker:peers(C, [A, B, D]),
	worker:peers(D, [A, B, C]),
	timer:sleep(5000),
	loggy:stop(Log),
	worker:stop(A),
	worker:stop(B),
	worker:stop(C),
	worker:stop(D).