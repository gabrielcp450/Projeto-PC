-module(matchmaker).
-export([start/0, find/1, cancel/1]).

start() ->
    Map = #{},
    Pid = spawn(fun() -> matchmaker(Map) end),
    register(?MODULE, Pid).

find(User) ->
    Level = stats:get_level(User),
    ?MODULE ! {self(), find, Level, User}.

cancel(User) ->
    Level = stats:get_level(User),
    io:format("Level of ~p is ~p~n", [User, Level]),
    ?MODULE ! {self(), cancel, Level}.

find_lvl(M, L) ->
    case maps:find(L, M) of
        {ok, _} ->
                erlang:display("Player found"),
                L;
        error -> 
            case maps:find(L + 1, M) of
                {ok, _} ->
                    erlang:display("Player found"),
                    L+1;
                error -> 
                    case maps:find(L - 1, M) of 
                        {ok, _} ->
                            erlang:display("Player found"),
                            L-1;
                        error ->
                            not_found
                    end
            end          
    end.

matchmaker(Map) ->
    receive 
        {Pid1, find, L, User1} -> 
            case find_lvl(Map, L) of
                not_found ->
                    erlang:display("Opponent not found, getting into queue"),
                    matchmaker(maps:put(L, {Pid1, User1}, Map));
                Level ->
                    erlang:display("Found match"),
                    {ok, {Pid2, User2}}  = maps:find(Level, Map),
                    match:create(Pid1, Pid2),
                    Pid1 ! {match_found, 1, User2},
                    Pid2 ! {match_found, 2, User1},
                    matchmaker(maps:remove(Level, Map))
            end;
        {Pid, cancel, L} ->
            Pid ! {match_cancelled},
            matchmaker(maps:remove(L, Map))
    end.
