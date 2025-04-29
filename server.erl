-module(server).
-export([start/0, stop/0]).

start() -> 
    Pid = spawn(fun() -> server(12345) end),
    register(server, Pid),
    Pid.

stop() -> server ! stop.

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    spawn(fun() -> acceptor(LSock) end),
    receive stop -> ok end.

acceptor(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  gen_tcp:send(Sock, "hello from the server\n"),
  spawn(fun() -> acceptor(LSock) end),
  loop(Sock).

loop(Sock) ->
    receive
        {tcp, _, Data} ->
            io:format("received msg: ~p~n", [Data]),
            gen_tcp:send(Sock, "pong\n"),
            loop(Sock);
        {tcp_closed, _} ->
            io:format("listener socket closed.~n");
        {tcp_error, _} ->
            io:format("error.~n")
    end.