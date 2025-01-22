-module(vtest).
-export([run/2]).

run(Sleep, Jitter) ->
	Nodes = [a, b, c, d],
	Log = vloggy:start(Nodes),
	A = vworker:start(a, Log, 13, Sleep, Jitter, Nodes),
	B = vworker:start(b, Log, 23, Sleep, Jitter, Nodes),
	C = vworker:start(c, Log, 36, Sleep, Jitter, Nodes),
	D = vworker:start(d, Log, 49, Sleep, Jitter, Nodes),
	vworker:peers(A, [B, C, D]),
	vworker:peers(B, [A, C, D]),
	vworker:peers(C, [A, B, D]),
	vworker:peers(D, [A, B, C]),
	timer:sleep(5000),
	vloggy:stop(Log),
	vworker:stop(A),
	vworker:stop(B),
	vworker:stop(C),
	vworker:stop(D).