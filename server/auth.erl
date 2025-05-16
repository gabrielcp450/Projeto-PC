-module(auth).
-export([create_account/2, close_account/1, login/2, logout/1, change_pass/2, start/0, save/0, stop/0, online/0]).

create_account(User, Pass) ->
    case rpc({create_account, User, Pass}) of 
        ok ->
            stats:insert(User),
            ok;
        user_exists ->
            user_exists
    end.

close_account(User) ->
    rpc({close_account, User}).

login(User, Pass) ->
    rpc({login, User, Pass}).

logout(User) ->
    rpc({logout, User}).

change_pass(User, New_Pass) ->
    rpc({change_pass, User, New_Pass}).

online() ->
    rpc({online}).

stop() ->
    rpc({stop}).

save() ->
    rpc({save}).

start() ->
    case file:read_file("storage/auth.bin") of 
        {ok, Bin} -> 
            Map = binary_to_term(Bin);
        {error, _ } ->
            Map = #{}
    end,
    Pid = spawn(fun() -> loop(Map) end),
    register(?MODULE, Pid).

loop(Map) -> 
    receive 
        {Pid, {create_account, U, P}} ->
            case maps:find(U, Map) of
                {ok, _} -> 
                    erlang:display("exists"),
                    Pid ! {create_account, user_exists},
                    loop(Map);
                error ->
                    NewMap = maps:put(U, {P, false}, Map),
                    erlang:display("ok"),
                    Pid ! {create_account, ok},
                    loop(NewMap)
            end;
        {Pid, {close_account, U}} -> 
            case maps:find(U, Map) of 
                {ok, _} ->
                    erlang:display("ok"),
                    NewMap = maps:remove(U, Map),
                    Pid ! {close_account, ok},
                    loop(NewMap);
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! {close_account, invalid},
                    loop(Map)
            end;
        {Pid, {login, U, P}} -> 
            case maps:find(U, Map) of 
                {ok, {P, false}} -> 
                    erlang:display("ok"),
                    NewMap = maps:update(U, {P, true}, Map),
                    Pid ! {login, ok},
                    loop(NewMap);
                {ok, {P, true}} -> 
                    erlang:display("User's already login"),
                    Pid ! {login, already_logged},
                    loop(Map);
                {ok, _} -> 
                    erlang:display("Invalid password"),
                    Pid ! {login, invalid},
                    loop(Map);
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! {login, invalid},
                    loop(Map)
            end;
        {Pid, {logout, U}} -> 
            case maps:find(U, Map) of
                {ok, {_, false}} ->
                    erlang:display("User is already logout"),
                    Pid ! {logout, invalid},
                    loop(Map);
                {ok, {P, true}} -> 
                    erlang:display("ok"),
                    Pid ! {logout, ok},
                    NewMap = maps:update(U, {P, false}, Map),
                    loop(NewMap);
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! {logout, invalid},
                    loop(Map)
            end;
        {Pid, {change_pass, U, NP}} ->
            case maps:find(U, Map) of
                {ok, {_, false}} ->
                    erlang:display("User is logout, can't change pass"),
                    Pid ! {change_pass, invalid},
                    loop(Map);
                {ok, {_, true}} -> 
                    erlang:display("ok"),
                    Pid ! {change_pass, ok},
                    NewMap = maps:update(U, {NP,true}, Map),
                    loop(NewMap);
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! {change_pass, invalid},
                    loop(Map)
            end;
        {Pid, {online}} -> 
            NewMap = maps:filtermap(is_online, map),
            Pid ! {online, maps:keys(NewMap)},
            loop(Map);
        {Pid, {save}} ->
            Bin = term_to_binary(Map),
            file:write_file("storage/auth.bin", Bin),
            Pid ! {save},
            loop(Map);
        {Pid, {stop}} ->
            Bin = term_to_binary(Map),
            file:write_file("storage/auth.bin", Bin),
            Pid ! {stop}
    end.

rpc(Request) ->
    ?MODULE ! {self(), Request},
    Tag = case Request of
        {T} -> T;
        {T, _} -> T;
        {T, _, _} -> T
    end,
    receive
        {Tag} -> true;
        {Tag, Msg} -> Msg
    end.
