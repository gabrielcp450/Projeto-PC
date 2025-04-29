-module(match_maker).
-export([find_match/1, start/0]).

start() ->
    Map = #{},
    Pid = spawn(fun() -> match_maker(Map) end),
    register(?MODULE, Pid).

find_match(Level) ->
    ?MODULE ! {self(), find_match, Level},
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
match_maker(Map) ->
    receive 
        {Pid, find_match, L} -> 
            case find_lvl(Map, L) of
                not_found ->
                    erlang:display("Player not found, getting into queue"),
                    New_Map = maps:put(L, Pid, Map),
                    match_maker(New_Map);
                Level ->
                    erlang:display("Found match"),
                    P  = maps:find(Level, Map),
                    New_Map = maps:remove(Level, Map),
                    P ! found_match,
                    Pid ! found_match,
                    match_maker(New_Map)
                    end
            end.
