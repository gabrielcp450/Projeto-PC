-module(movement).
-export([
    player_movement/1,
    proj_movement/1
]).
-import(utils, [
    normalize/1,
    add/2,
    mul/2
]).

-include("match.hrl").

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

move_player(Player) -> 
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

move_projectile(Proj) ->
    Dt = ?TICK / 1000,
    Proj#proj{p = add(Proj#proj.p, mul(Proj#proj.v, Dt))}.

proj_movement(Pids) ->
    maps:map(fun(_, Player) ->
        Player#player{projs = maps:map(fun(_, Proj) -> move_projectile(Proj) end, Player#player.projs)}
    end, Pids).

player_movement(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),
    #{Pid1 => move_player(Player1), Pid2 => move_player(Player2)}.