-module(match).
-export([create/2]).

-record(player, {p = {0, 0}, v = {0.1, 0}, a = {0, 0}, points = 0, proj_v = 0, proj_i = 0 }).
-define(TICK, 500).

initial_pos(I) ->
    Px = 1/2 * (I+1) - 1/4,
    #player{p = {Px, 1/2}}.

movement_player(Player) -> 
    {Px, Py} = Player#player.p,
    {Vx, Vy} = Player#player.v,
    {Ax, Ay} = Player#player.a,

    Dt = ?TICK/1000,

    % Update velocity
    NewVx = Vx + Ax*Dt,
    NewVy = Vy + Ay*Dt,

    % Update position
    % Position equation Δx = v0​⋅dt + 0.5⋅a⋅dt^2
    NewPx = Px + NewVx*Dt + 0.5 * Ax * Dt*Dt,
    NewPy = Py + NewVy*Dt + 0.5 * Ay * Dt*Dt,

    Player#player{p = {NewPx, NewPy}, v = {NewVx, NewVy}}.

movement(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),
    #{Pid1 => movement_player(Player1), Pid2 => movement_player(Player2)}.

create(Pid1, Pid2) -> 
    register(player1, Pid1),
    register(player2, Pid2),
    Pids = #{Pid1 => initial_pos(0), Pid2 => initial_pos(1)},
    Pid = spawn(fun() -> loop(Pids) end),
    timer:send_after(5000, Pid, finished).

loop(Pids) ->
    timer:send_after(?TICK, self(), update),

    receive 
        update -> 
            NewPids = movement(Pids),
            Player1 = maps:get(player1, NewPids),
            Player2 = maps:get(player2, NewPids),
            {X1, Y1} = Player1#player.p,
            {X2, Y2} = Player2#player.p,
            if 
                X1 =< 0 orelse X1 >= 1 orelse Y1 =< 0 orelse Y1 >= 1 ->
                    Points = Player2#player.points,
                    Player1New = Player1#player{p = (initial_pos(0))#player.p},
                    Player2New = Player2#player{
                        p = (initial_pos(1))#player.p,
                        points = Points + 2
                    },
                    UpdatedPids = NewPids#{player1 => Player1New, player2 => Player2New},
                    [Pid ! {pos, Player#player.p} || {Pid, Player} <- maps:to_list(UpdatedPids)],
                    loop(UpdatedPids);
                X2 =< 0 orelse X2 >= 1 orelse Y2 =< 0 orelse Y2 >= 1 ->
                    Points = Player1#player.points,
                    Player2New = Player1#player{p = (initial_pos(1))#player.p},
                    Player1New = Player2#player{
                        p = (initial_pos(0))#player.p,
                        points = Points + 2
                    },
                    UpdatedPids = NewPids#{player1 => Player1New, player2 => Player2New},
                    [Pid ! {pos, Player#player.p} || {Pid, Player} <- maps:to_list(UpdatedPids)],
                    loop(UpdatedPids)
                end,

            [Pid ! {pos, Player#player.p} || {Pid, Player} <- maps:to_list(NewPids)],
            loop(NewPids);
        finished ->
            [{Pid1, _}, {Pid2, _}] = maps:to_list(Pids),
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
