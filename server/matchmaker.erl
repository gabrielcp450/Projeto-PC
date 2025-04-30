-module(matchmaker).
-export([find/1, start/0]).

start() ->
    Map = #{},
    Pid = spawn(fun() -> matchmaker(Map) end),
    register(?MODULE, Pid).

find(Level) ->
    ?MODULE ! {self(), find, Level},
    % receive match pid
    receive Msg -> Msg end.

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
        {Player1, find, L} -> 
            case find_lvl(Map, L) of
                not_found ->
                    erlang:display("Player not found, getting into queue"),
                    matchmaker(maps:put(L, Player1, Map));
                Level ->
                    erlang:display("Found match"),
                    {ok, Player2}  = maps:find(Level, Map),
                    Match = match:create([Player1, Player2]),
                    Player1 ! {found, Match},
                    Player2 ! {found, Match},
                    matchmaker(maps:remove(Level, Map))
            end
    end.
