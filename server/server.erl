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
                        ok -> 
                            auth:save(),
                            stats:save(),
                            gen_tcp:send(Sock, "!ok\n");
                        user_exists -> gen_tcp:send(Sock, "username already used\n")
                    end;
                ["/l", User, Pass] ->
                    case auth:login(User, Pass) of
                        ok ->
                            gen_tcp:send(Sock, "!ok\n"),
                            user_logged_in(Sock, User);
                        already_logged ->
                            gen_tcp:send(Sock, "user already logged in\n");
                        invalid ->
                            gen_tcp:send(Sock, "wrong username or password\n")
                    end;
                ["/save"] -> 
                    auth:save(),
                    stats:save(),
                    gen_tcp:send(Sock, "!ok\n");
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
                            gen_tcp:send(Sock, "!ok\n"),
                            user_logged_out(Sock);
                        invalid -> gen_tcp:send(Sock, "user not logged\n")
                    end;
                ["/d"] ->
                    case auth:close_account(User) of
                        ok -> 
                            gen_tcp:send(Sock, "!ok\n"),
                            user_logged_out(Sock);
                        invalid -> gen_tcp:send(Sock, "user not logged\n")
                    end;
                ["/c", NewPass] ->
                    case auth:change_pass(User, NewPass) of
                        ok -> gen_tcp:send(Sock, "!ok\n");
                        invalid -> gen_tcp:send(Sock, "user not logged\n")
                    end;
                ["/s"] ->
                    matchmaker:find(User);
                ["/s-"] ->
                    matchmaker:cancel(User);
                ["/t"] ->
                    Top10 = stats:top10(),
                    io:format("Top10: ~p~n", [Top10]),
                    Str = io_lib:format("Top10: ~p~n", [Top10]),
                    gen_tcp:send(Sock, lists:flatten(Str)),
                    user_logged_in(Sock, User);
                _ -> gen_tcp:send(Sock, "invalid message\n")
            end;
        {match_found, MyId, Opponent} -> 
            Str = io_lib:format("!found ~p ~p~n", [MyId, Opponent]),
            gen_tcp:send(Sock, lists:flatten(Str)),
            user_in_match(Sock, User);
        {match_cancelled} ->
            io:format("match search cancelled: ~p~n", [User]),
            gen_tcp:send(Sock, "!cancelled\n");
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


user_in_match(Sock, User) ->
    receive
        {player_pos, Id, {X, Y}} ->
            gen_tcp:send(Sock, io_lib:format("!player_pos ~p ~p ~p\n", [Id, X, Y])),
            user_in_match(Sock, User);
        {finished, Result} ->
            io:format("match finished~n"),
            gen_tcp:send(Sock, io_lib:format("!finished ~p\n", [Result])),
            % case Result of
            %     win -> stats:add_win(User);
            %     loss -> stats:add_loss(User)
            % end,
            user_logged_in(Sock, User);
        {tcp_closed, _} ->
            io:format("socket closed when in match.~n"),
            auth:logout(User),
            user_logged_out(Sock);
        {tcp_error, _} ->
            io:format("error when in match.~n"),
            auth:logout(User),
            user_logged_out(Sock)
    end.
