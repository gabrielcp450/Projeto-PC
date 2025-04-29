-module(match).
-export([create/1]).

create(Pids) -> 
    Map = maps:from_list(lists:map(fun(U) -> {U, 0} end, Pids)),
    spawn(fun() -> loop(Map) end).

loop(Map) ->
    % erlang:send_after(2*60*1000, self(), finished),
    erlang:send_after(2000, self(), finished),
    receive 
        ping -> 
            [Pid ! {pos, 4, 2} || Pid <- maps:keys(Map)],
            loop(Map);
        finished ->
            [{Pid1, _}, {Pid2, _}] = maps:to_list(Map),
            % continue as before
            Points1 = 2,
            Points2 = 4,
            {Result1, Result2} = if 
                Points1 < Points2 -> 
                    {loss, win};
                Points1 > Points2 ->
                    {win, loss};
                true ->
                    {draw, draw}
            end,
            Pid1 ! {finished, Result1},
            Pid2 ! {finished, Result2}
    end.
