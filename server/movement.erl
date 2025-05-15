-module(movement).
-export([
    player_movement/1,
    proj_movement/1
]).
-import(utils, [
    normalize/1
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

proj_movement(Projs) ->
    maps:map(fun(_, Proj) -> 
        {X, Y} = Proj#proj.p,
        {Vx, Vy} = Proj#proj.v,
        Dt = ?TICK / 1000,
        NewX = X + Vx * Dt,
        NewY = Y + Vy * Dt,
        Proj#proj{p = {NewX, NewY}}
    end, Projs).

player_movement(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),
    #{Pid1 => move_player(Player1), Pid2 => move_player(Player2)}.