-module(matchmaker).
-export([start/0, find/1, cancel/1]).

start() ->
    Map = #{},
    Pid = spawn(fun() -> matchmaker(Map) end),
    register(?MODULE, Pid).

find(User) ->
    io:format("Received find for user: ~p~n", [User]),
    case stats:get_level(User) of
        undefined -> io:format("User ~p has undefined level~n", [User]);
        Level when is_integer(Level) ->
            io:format("Level of ~p is ~p~n", [User, Level]),
            ?MODULE ! {self(), find, Level, User}
    end.

cancel(User) ->
    io:format("User cancelled match searching: ~p~n", [User]),
    case stats:get_level(User) of
        undefined -> io:format("User ~p has undefined level~n", [User]);
        Level when is_integer(Level) ->
            io:format("Level of ~p is ~p~n", [User, Level]),
            ?MODULE ! {self(), cancel, Level}
    end.

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
                    Match = match:create(Pid1, Pid2),
                    Pid1 ! {match_found, Match, 0, User2},
                    Pid2 ! {match_found, Match, 1, User1},
                    matchmaker(maps:remove(Level, Map))
            end;
        {Pid, cancel, L} ->
            Pid ! {match_cancelled},
            matchmaker(maps:remove(L, Map))
    end.
