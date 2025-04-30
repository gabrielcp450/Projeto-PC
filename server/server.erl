-module(server).
-export([start/0, stop/0]).

start() -> 
    Pid = spawn(fun() -> server(12345) end),
    register(server, Pid),
    Pid.

stop() -> server ! stop.

server(Port) ->
    auth:start(),
    matchmaker:start(),
    {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    spawn(fun() -> acceptor(LSock) end),
    receive stop -> ok end.

acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    gen_tcp:send(Sock, "hello from the server\n"),
    spawn(fun() -> acceptor(LSock) end),
    user_logged_out(Sock).

user_logged_out(Sock) ->
    receive 
        {tcp, _, Data} -> 
            case string:tokens(Data, " \n") of
                ["/c", User, Pass] ->
                    case auth:create_account(User, Pass) of
                        ok -> gen_tcp:send(Sock, "user created\n");
                        user_exists -> gen_tcp:send(Sock, "username already used\n")
                    end;
                ["/l", User, Pass] ->
                    case auth:login(User, Pass) of
                        ok ->
                            gen_tcp:send(Sock, "user logged in\n"),
                            user_logged_in(Sock, User);
                        already_logged ->
                            gen_tcp:send(Sock, "user already logged in\n");
                        invalid ->
                            gen_tcp:send(Sock, "wrong username or password\n")
                    end;
                ["/save"] -> auth:save();
                _ -> gen_tcp:send(Sock, "invalid message\n")
            end;
        {tcp_closed, _} ->
            io:format("socket closed.~n");
        {tcp_error, _} ->
            io:format("error.~n")
    end,
    user_logged_out(Sock).

user_logged_in(Sock, User) ->
    receive 
        {tcp, _, Data} -> 
            case string:tokens(Data, " \n") of
                ["/e"] ->
                    io:format("hello ~p~n", [User]),
                    case auth:logout(User) of
                        ok -> 
                            gen_tcp:send(Sock, "user logged out\n"),
                            user_logged_out(Sock);
                        invalid -> gen_tcp:send(Sock, "user not logged\n")
                    end;
                ["/d"] ->
                    case auth:close_account(User) of
                        ok -> 
                            gen_tcp:send(Sock, "user deleted\n"),
                            user_logged_out(Sock);
                        invalid -> gen_tcp:send(Sock, "user not logged\n")
                    end;
                ["/c", NewPass] ->
                    case auth:change_pass(User, NewPass) of
                        ok -> gen_tcp:send(Sock, "user password changed\n");
                        invalid -> gen_tcp:send(Sock, "user not logged\n")
                    end;
                ["/s"] ->
                    matchmaker:find(5),
                    user_in_match(Sock, User);
                _ -> gen_tcp:send(Sock, "invalid message\n")
            end;
        {tcp_closed, _} ->
            io:format("socket closed.~n"),
            user_logged_out(Sock);
        {tcp_error, _} ->
            io:format("error.~n"),
            user_logged_out(Sock)
    end,
    user_logged_in(Sock, User).


user_in_match(Sock, User) ->
    receive
        {pos, X, Y} ->
            io:format("STREAMING: x:~p y:~p~n", [X, Y]),
            gen_tcp:send(Sock, io_lib:format("x:~p y:~p\n", [X, Y]));
        {finished, Result} ->
            gen_tcp:send(Sock, "END\n"),
            gen_tcp:send(Sock, io_lib:format("match finished: ~p\n", [Result])),
            user_logged_in(Sock, User)
    end.
