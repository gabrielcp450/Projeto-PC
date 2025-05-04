-module(matchmaker).
-export([find/1, start/0]).

start() ->
    Map = #{},
    Pid = spawn(fun() -> matchmaker(Map) end),
    register(?MODULE, Pid).

find(User) ->
    Level = stats:get_level(User),
    ?MODULE ! {self(), find, Level, User}.

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
                    Pid1 ! {match_found, Match, User2},
                    Pid2 ! {match_found, Match, User1},
                    matchmaker(maps:remove(Level, Map))
            end
    end.
