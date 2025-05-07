-module(server).
-export([start/0, stop/0]).

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
                ["/save"] -> 
                    auth:save(),
                    stats:save();
                _ -> gen_tcp:send(Sock, "invalid message\n")
            end;
        {tcp_closed, _} ->
            io:format("socket closed when logged out.~n");
        {tcp_error, _} ->
            io:format("error when logged out.~n")
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
                    matchmaker:find(User),
                    user_logged_in(Sock, User);
                ["/t"] ->
                    Top10 = stats:top10(),
                    io:format("Top10: ~p~n", [Top10]),
                    Str = io_lib:format("Top10: ~p~n", [Top10]),
                    gen_tcp:send(Sock, lists:flatten(Str)),
                    user_logged_in(Sock, User);
                _ -> gen_tcp:send(Sock, "invalid message\n")
            end;
        {match_found, Match, Opponent} -> 
            Str = io_lib:format("match found: ~p vs ~p~n", [User, Opponent]),
            gen_tcp:send(Sock, lists:flatten(Str)),
            user_in_match(Sock, Match, User);
        {tcp_closed, _} ->
            io:format("socket closed when logged in.~n"),
            auth:logout(User),
            user_logged_out(Sock);
        {tcp_error, _} ->
            io:format("error when logged in.~n"),
            auth:logout(User),
            user_logged_out(Sock)
    end,
    user_logged_in(Sock, User).


user_in_match(Sock, Match, User) ->
    receive
        {pos, Pos} ->
            io:format("STREAMING: pos:~p~n", [Pos]),
            %gen_tcp:send(Sock, io_lib:format("pos:~p\n", [Pos])),
            user_in_match(Sock, Match, User);
        {finished, Result} ->
            gen_tcp:send(Sock, "END\n"),
            gen_tcp:send(Sock, io_lib:format("match finished: ~p\n", [Result])),
            case Result of
                win -> stats:add_win(User);
                loss -> stats:add_loss(User)
            end,
            user_logged_in(Sock, User);
        {tcp,_, Data} ->
            case string:tokens(Data, " \n") of
                ["/pressed", L] ->
                    erlang:display("hello"),
                    Match !{self(), pressed, L};
                ["/unpressed", L] ->
                    Match ! {self(), unpressed, L};
                _ ->
                    io:format("Received unknown command: ~p~n", [Data])
            end,
            user_in_match(Sock, Match, User);

        {tcp_closed, _} ->
            io:format("socket closed when in match.~n"),
            auth:logout(User),
            user_logged_out(Sock);
        {tcp_error, _} ->
            io:format("error when in match.~n"),
            auth:logout(User),
            user_logged_out(Sock)
    end.
