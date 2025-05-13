-module(match).
-export([create/2]).

-record(keys, {w = false, s = false, a = false, d = false}).
-record(player, {p = {0, 0}, v = {0, 0}, a = {0, 0}, points = 0, proj_v = 0, proj_i = 0, k = #keys{}, first = 0}).
-define(TICK, 1).
-define(FRICTION, 20.0).
-define(ACCELERATION, 20.0).
-define(MODEFIERS, 100).
-define(MAX_MODIFIERS, 2).

initial_pos(I) ->
    Px = 1/2 * (I+1) - 1/4,
    #player{p = {Px, 1/2}, first = I}.

normalize({X, Y}) ->
    Length = math:sqrt(X*X + Y*Y),
    case Length > 0 of
        true -> {X/Length, Y/Length};
        false -> {0, 0}
    end.

direction(Player) ->
    Keys = Player#player.k,
    W = Keys#keys.w,
    S = Keys#keys.s,
    D = Keys#keys.d,
    A = Keys#keys.a,

    H = case {D, A} of 
        {true, false} ->
            1;
        {false, true} ->
            -1;
        _ ->
            0
        end,
    V = case {W, S} of
        {true, false} ->
            -1;
        {false, true} ->
            1;
        _ ->
            0 
        end,
    normalize({H, V}).
        
acceleration(Player) -> 
    {H, V} = direction(Player),
    {H * ?ACCELERATION, V * ?ACCELERATION}.

movement_player(Player) -> 
    {Px, Py} = Player#player.p,
    {Vx, Vy} = Player#player.v,
    {Ax, Ay} = acceleration(Player),

    Dt = ?TICK / 1000,

    % Calculate friction (opposes current velocity)
    FrictionX = -?FRICTION * Vx,
    FrictionY = -?FRICTION * Vy,

    % Update velocity with both acceleration and friction
    NewVx = Vx + (Ax + FrictionX)*Dt,
    NewVy = Vy + (Ay + FrictionY)*Dt,

    % Update position
    % Position equation Δx = v0​⋅dt + 0.5⋅a⋅dt^2
    NewPx = Px + NewVx*Dt + 0.5*(Ax + FrictionX)*Dt*Dt,
    NewPy = Py + NewVy*Dt + 0.5*(Ay + FrictionY)*Dt*Dt,

    Player#player{p = {NewPx, NewPy}, v = {NewVx, NewVy}, a = {0, 0}}.

movement(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),
    #{Pid1 => movement_player(Player1), Pid2 => movement_player(Player2)}.
gen_random() ->
    rand:uniform(99)/100.

gen_modifiers(Pids,Mod) ->
    L = [M || {M, X} <- maps:to_list(Mod), length(X) < ?MAX_MODIFIERS],
    case L of 
        [] ->
            Mod;
        _ ->
            Nt = rand:uniform(length(L)),
            Choosen = lists:nth(Nt, L),
            C = {gen_random(), gen_random()},
            [Pid ! {modifier_pos, Choosen, C} || Pid <- maps:keys(Pids)],
            S = maps:get(Choosen, Mod),
            maps:update(Choosen, S ++ [C], Mod)
    end.

        

create(Pid1, Pid2) -> 
    Pids = #{Pid1 => initial_pos(0), Pid2 => initial_pos(1)},
    Mod = # {0 => [], 1 => [], 2 => [], 3 => []},
    MatchPid = spawn(fun() -> loop(Pids, Mod) end),
    timer:send_after(5000000, MatchPid, finished),
    timer:send_after(?TICK, MatchPid, update),
    timer:send_after(?MODEFIERS, MatchPid, modifiers),
    MatchPid. 

pressed(Pid, Pids, K) ->
    Player = maps:get(Pid, Pids),
    Keys = Player#player.k,
    NewKeys = case K of
        "w" -> Keys#keys{w = true};
        "s" -> Keys#keys{s = true};
        "a" -> Keys#keys{a = true};
        "d" -> Keys#keys{d = true};
        _ -> Keys
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
        "d" -> Keys#keys{d = false};
        _ -> Keys
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
            % Send score update to both players
            FirstPid ! {score, FirstPlayerNew#player.points, SecondPlayerNew#player.points},
            SecondPid ! {score, SecondPlayerNew#player.points, FirstPlayerNew#player.points},
            Pids#{FirstPid => FirstPlayerNew, SecondPid => SecondPlayerNew};
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
            % Send score update to both players
            FirstPid ! {score, FirstPlayerNew#player.points, SecondPlayerNew#player.points},
            SecondPid ! {score, SecondPlayerNew#player.points, FirstPlayerNew#player.points},
            Pids#{FirstPid => FirstPlayerNew, SecondPid => SecondPlayerNew};
        true ->
            Pids
    end.


loop(Pids, Mod) ->
    receive 
        update -> 
            NewPids = movement(Pids),
            NewPids2 = collision_walls(NewPids),
            Players = maps:keys(NewPids2),
            [Pid ! {player_pos, Player#player.first, Player#player.p} || Player <- maps:values(NewPids2), Pid <- Players ],
            timer:send_after(?TICK, self(), update),
            loop(NewPids2, Mod);
        modifiers ->
            NewMod = gen_modifiers(Pids, Mod),
            timer:send_after(?MODEFIERS, self(), modifiers),
            loop(Pids, NewMod);
        {Pid, pressed, K} ->
            NewPlayer = pressed(Pid, Pids, K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod);
        {Pid, unpressed, K} ->
            NewPlayer = unpressed(Pid, Pids, K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod);
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
