-module(server).
-export([start/0, stop/0]).

start() -> 
    Pid = spawn(fun() -> server(12345) end),
    register(server, Pid),
    Pid.

stop() -> server ! stop.

server(Port) ->
    auth:start(),
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
                        invalid ->
                            gen_tcp:send(Sock, "wrong username or password\n")
                    end;
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
                _ -> gen_tcp:send(Sock, "invalid message\n")
                % ["/s"] ->
                %     case match:search(User) of
                %         ok -> ok
                %     end
            end;
        {tcp_closed, _} ->
            io:format("socket closed.~n");
        {tcp_error, _} ->
            io:format("error.~n")
    end,
    user_logged_in(Sock, User).