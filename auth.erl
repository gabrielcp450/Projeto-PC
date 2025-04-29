-module(auth).
-export([create_account/2, close_account/1, login/2, logout/1, change_pass/2, start/0, save/0, stop/0, online/0]).



create_account(User ,Pass ) ->
    ?MODULE ! {self(), create_account, User, Pass},
    receive Msg -> Msg end.


close_account(User) ->
    ?MODULE ! {self(), close_account, User},
    receive Msg -> Msg end.


login(User, Pass) ->
    ?MODULE ! {self(), login, User, Pass},
    receive Msg -> Msg end.

logout(User) ->
    ?MODULE ! {self(), logout, User},
    receive Msg -> Msg end.

change_pass(User, New_Pass) ->
    ?MODULE ! {self(), change_pass, User,New_Pass},
    receive Msg -> Msg end.

online() ->
    ?MODULE ! {self(), online},
    receive Msg -> Msg end.



start() ->
    case file:read_file("map_file.bin") of 
        {ok, Bin} -> 
            Map = binary_to_term(Bin);
        {error, _ } ->
            Map = #{}
    end,
    Pid = spawn(fun() -> loop(Map) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

save() ->
    ?MODULE ! save.

loop(Map) -> 
    receive 
        {Pid, create_account, U, P} ->
            case maps:find(U, Map) of
                {ok, _ } -> 
                    erlang:display("exists"),
                    Pid ! user_exists,
                    loop(Map);
               error ->
                    NewMap = maps:put(U,{P, false}, Map),
                    erlang:display("ok"),
                    Pid ! ok,
                    loop(NewMap)
            end;
        {Pid, close_account, U} -> 
            case maps:find(U, Map) of 
                {ok,_} ->
                    erlang:display("ok"),
                    Pid ! ok,
                    NewMap = maps:remove(U, Map),
                    loop(NewMap);
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! invalid,
                    loop(Map)
            end;
        {Pid, login, U, P} -> 
            case maps:find(U, Map) of 
                {ok, {P, false} } -> 
                    erlang:display("ok"),
                    Pid ! ok,
                    NewMap = maps:update(U,{P, true}, Map),
                    loop(NewMap);
                {ok, {P, true}} -> 
                    erlang:display("User's already login"),
                    Pid ! invalid,
                    loop(Map);
                {ok, _} -> 
                    erlang:display("Invalid password"),
                    Pid ! invalid,
                    loop(Map);
 
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! invalid,
                    loop(Map)
            end;
        {Pid, logout, U} -> 
            case maps:find(U, Map) of
                {ok, {_, false}} ->
                    erlang:display("User is already logout"),
                    Pid ! invalid,
                    loop(Map);
                {ok, {P, true} } -> 
                    erlang:display("ok"),
                    Pid ! ok,
                    NewMap = maps:update(U,{P, false}, Map),
                    loop(NewMap);
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! invalid,
                    loop(Map)
            end;
        {Pid, change_pass, U, NP} ->
            case maps:find(U, Map) of
                {ok, {_, false}} ->
                    erlang:display("User is logout, can't change pass"),
                    Pid ! invalid,
                    loop(Map);
                {ok, {_, true} } -> 
                    erlang:display("ok"),
                    Pid ! ok,
                    NewMap = maps:update(U,{NP,true}, Map),
                    loop(NewMap);
 
                error ->
                    erlang:display("Dosen't exist"),
                    Pid ! invalid,
                    loop(Map)
            end,
            Pid ! invalid;
        {Pid, online} -> 
                NewMap = maps:filtermap(is_online, map),
                Pid ! maps:keys(NewMap),
                % Pid ! [U || {U , {_, true}} <- to_list(map)]
                loop(Map);
        save ->
            Bin = term_to_binary(Map),
            file:write_file("map_file.bin", Bin),
            loop(Map);


        stop ->
            Bin = term_to_binary(Map),
            file:write_file("map_file.bin", Bin)

    end.

