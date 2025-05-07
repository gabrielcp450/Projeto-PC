-module(match).
-export([create/2]).

-record(keys, {w = false, s = false, a = false, d = false}).
-record(player, {p = {0, 0}, v = {0.1, 0}, a = {0, 0}, points = 0, proj_v = 0, proj_i = 0, k = #keys{}}).
-define(TICK, 50).

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
    Pids = #{player1 => initial_pos(0), player2 => initial_pos(1)},
    Pid = spawn(fun() -> loop(Pids) end),
    timer:send_after(5000, Pid, finished).

pressed(Pid, Pids, K) ->
    Player = maps:get(Pid, Pids),
    Keys = Player#player.k,
    NewKeys = case K of
        w -> Keys#keys{w = true};
        s -> Keys#keys{s = true};
        a -> Keys#keys{a = true};
        d -> Keys#keys{d = true}
    end,
    NewPlayer = Player#player{k = NewKeys},
    NewPlayer.

unpressed(Pid, Pids, K) ->
    Player = maps:get(Pid, Pids),
    Keys = Player#player.k,
    NewKeys = case K of
        w -> Keys#keys{w = false};
        s -> Keys#keys{s = false};
        a -> Keys#keys{a = false};
        d -> Keys#keys{d = false}
        end,
    NewPlayer = Player#player{k = NewKeys},
    NewPlayer.

collision_walls(Pids) ->
    Player1 = maps:get(player1, Pids),
    Player2 = maps:get(player2, Pids),
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
            Pids#{player1 => Player1New, player2 => Player2New};
        X2 =< 0 orelse X2 >= 1 orelse Y2 =< 0 orelse Y2 >= 1 ->
            Points = Player1#player.points,
            Player2New = Player2#player{p = (initial_pos(1))#player.p},
            Player1New = Player1#player{
                p = (initial_pos(0))#player.p,
                points = Points + 2
            },
            Pids#{player1 => Player1New, player2 => Player2New};
        true ->
            Pids
    end.


loop(Pids) ->
    timer:send_after(?TICK, self(), update),

    receive 
        update -> 
            NewPids = movement(Pids),
            NewPids2 = collision_walls(NewPids),
            Player1 = maps:get(player1, NewPids2),
            Player2 = maps:get(player2, NewPids2),
            player1 ! {player_pos, 0, Player1#player.p},
            player1 ! {player_pos, 1, Player2#player.p},
            player2 ! {player_pos, 0, Player1#player.p},
            player2 ! {player_pos, 1, Player2#player.p},
            loop(NewPids2);
        {Pid, pressed, K} ->
            NewPlayer = pressed(Pid, Pids, K),
            NewPids = maps:put(Pid, NewPlayer, Pids),
            loop(NewPids);
        {Pid, unpressed, K} ->
            NewPlayer = unpressed(Pid, Pids, K),
            NewPids = maps:put(Pid, NewPlayer, Pids),
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
            Pid2 ! {finished, Result2},
            unregister(player1),
            unregister(player2)
    end.
