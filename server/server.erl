-module(server).
-export([start/0, stop/0]).
-import(user, [
    acceptor/1
]).

start() -> 
    Pid = spawn(fun() -> server(13556) end),
    register(server, Pid),
    Pid.

stop() -> server ! stop.

server(Port) ->
    auth:start(),
    matchmaker:start(),
    stats:start(),
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    spawn(fun() -> acceptor(LSock) end),
    receive stop -> ok end.
