-module(match).
-export([create/2]).

-record(keys, {w = false, s = false, a = false, d = false}).
-record(player, {p = {0, 0}, v = {0, 0}, a = {0, 0}, points = 0, proj_v = 0, proj_i = 0, k = #keys{}, first = 0}).
-define(TICK, 9000).

initial_pos(I) ->
    Px = 1/2 * (I+1) - 1/4,
    #player{p = {Px, 1/2}, first = I}.

accelaration(Player) -> 
    {Ax, Ay} = Player#player.a,
    Keys = Player#player.k,
    W = Keys#keys.w,
    S = Keys#keys.s,
    D = Keys#keys.d,
    A = Keys#keys.a,

    NewAx = case {D, A} of 
        {true, false} ->
            Ax + 0.001;
        {false, true} ->
            Ax -0.001;
        _ ->
            Ax
        end,
    NewAy = case {W, S} of
        {true, false} ->
            Ay + 0.001;
        {false, true} ->
            Ay -0.001;
        _ ->
            Ay
        end,
    {NewAx, NewAy}.
        
movement_player(Player) -> 
    
    {Px, Py} = Player#player.p,
    {Vx, Vy} = Player#player.v,
    {Ax, Ay} = accelaration(Player),

    Dt = ?TICK/1000,

    % Update velocity
    NewVx = Vx + Ax*Dt,
    NewVy = Vy + Ay*Dt,

    % Update position
    % Position equation Δx = v0​⋅dt + 0.5⋅a⋅dt^2
    NewPx = Px + NewVx*Dt + 0.5 * Ax * Dt*Dt,
    NewPy = Py + NewVy*Dt + 0.5 * Ay * Dt*Dt,

    Player#player{p = {NewPx, NewPy}, v = {NewVx, NewVy}, a = {Ax, Ay}}.

movement(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),
    #{Pid1 => movement_player(Player1), Pid2 => movement_player(Player2)}.

create(Pid1, Pid2) -> 
    Pids = #{Pid1 => initial_pos(0), Pid2 => initial_pos(1)},
    MatchPid = spawn(fun() -> loop(Pids) end),
    timer:send_after(5000000, MatchPid, finished),
    MatchPid. 

pressed(Pid, Pids, K) ->
    Player = maps:get(Pid, Pids),
    Keys = Player#player.k,
    NewKeys = case K of
        "w" -> Keys#keys{w = true};
        "s" -> Keys#keys{s = true};
        "a" -> Keys#keys{a = true};
        "d" -> Keys#keys{d = true}
    end,
    NewPlayer = Player#player{k = NewKeys},
    NewPlayer.

unpressed(Pid, Pids, K) ->
    Player = maps:get(Pid, Pids),
    Keys = Player#player.k,
    NewKeys = case K of
        "w" -> Keys#keys{w = false};
        "s" -> Keys#keys{s = false};
        "a" -> Keys#keys{a = false};
        "d" -> Keys#keys{d = false}
        end,
    NewPlayer = Player#player{k = NewKeys},
    NewPlayer.

collision_walls(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),

    {FirstPlayer, FirstPid, SecondPlayer, SecondPid} = 
        case Player1#player.first of
            0 -> {Player1, Pid1, Player2, Pid2};
            1 -> {Player2, Pid2, Player1, Pid1}
        end,

    {X1, Y1} = FirstPlayer#player.p,
    {X2, Y2} = SecondPlayer#player.p,

    if 
        X1 =< 0 orelse X1 >= 1 orelse Y1 =< 0 orelse Y1 >= 1 ->
            erlang:display("Player1 collision"),
            Points = SecondPlayer#player.points,
            FirstPlayerNew = FirstPlayer#player{
                p = (initial_pos(0))#player.p,
                a = {0,0},
                v = {0,0},
                k = #keys{} },
            SecondPlayerNew = SecondPlayer#player{
                p = (initial_pos(1))#player.p,
                a = {0,0},
                v = {0,0},
                k = #keys{},
                points = Points + 2
            },
            UpdatedPids = Pids#{FirstPid => FirstPlayerNew, SecondPid => SecondPlayerNew},
            [Pid ! {pos, Player#player.p} || {Pid, Player} <- maps:to_list(UpdatedPids)],
            UpdatedPids;
        X2 =< 0 orelse X2 >= 1 orelse Y2 =< 0 orelse Y2 >= 1 ->
            erlang:display("Player2 collision"),
            Points = FirstPlayer#player.points,
            SecondPlayerNew = SecondPlayer#player{
                p = (initial_pos(1))#player.p,
                a = {0,0},
                v = {0,0},
                k = #keys{}},
            FirstPlayerNew = FirstPlayer#player{
                p = (initial_pos(0))#player.p,
                a = {0,0},
                v = {0,0},
                k = #keys{},
                points = Points + 2
            },
            UpdatedPids = Pids#{FirstPid => FirstPlayerNew, SecondPid => SecondPlayerNew},
            [Pid ! {pos, Player#player.p} || {Pid, Player} <- maps:to_list(UpdatedPids)],
            UpdatedPids;
        true ->
            [Pid ! {pos, Player#player.p} || {Pid, Player} <- maps:to_list(Pids)],
            Pids
    end.


loop(Pids) ->
    timer:send_after(?TICK, self(), update),

    receive 
        update -> 
            NewPids = movement(Pids),
            NewPids2 = collision_walls(NewPids),
            [Pid ! {pos, Player#player.p} || {Pid, Player} <- maps:to_list(NewPids2)],
            loop(NewPids2);
        {Pid, pressed, K} ->
            erlang:display("Hello1"),
            NewPlayer = pressed(Pid, Pids, K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids);
        {Pid, unpressed, K} ->
            erlang:display("Hello2"),
            NewPlayer = unpressed(Pid, Pids, K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
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
