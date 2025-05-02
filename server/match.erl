-module(match).
-export([create/1]).

create(Pids) -> 
    Map = maps:from_list(lists:map(fun(U) -> {U, 0} end, Pids)),
    Pid = spawn(fun() -> loop(Map) end),
    timer:send_after(5000, Pid, finished).

loop(Map) ->
    Player1Pos = {1, 1},
    Player2Pos = {4, 4},
    World = [[1, 1, 1, 1, 1, 1, 1],
             [1, 0, 0, 0, 0, 0, 1],
             [1, 0, 0, 0, 0, 0, 1],
             [1, 0, 0, 0, 0, 0, 1],
             [1, 0, 0, 0, 0, 0, 1],
             [1, 0, 0, 0, 0, 0, 1],
             [1, 1, 1, 1, 1, 1, 1]],

    timer:send_after(1000, self(), ping),

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
