-module(match).
-export([create/2, initial_pos/1]).
-import(movement, [
    player_movement/1,
    proj_movement/1
]).
-import(collision, [
    player_collision/1,
    proj_collision/2,
    mod_collision/2
]).
-import(projectile, [
    shoot/4,
    aim/3
]).
-import(keys, [
    pressed/2,
    unpressed/2
]).
-import(modifiers, [
    gen_modifiers/3
]).

-include("match.hrl").

initial_pos(I) ->
    Px = 1/2 * (I+1) - 1/4,
    #player{p = {Px, 1/2}, id = I}.

create(Pid1, Pid2) -> 
    Projs = [],
    Pids = #{Pid1 => initial_pos(0), Pid2 => initial_pos(1)},
    Mod = # {0 => [], 1 => [], 2 => [], 3 => []},
    MatchPid = spawn(fun() -> loop(Pids, Projs, Mod, 0) end),
    timer:send_after(5000000, MatchPid, finished),
    timer:send_after(?TICK, MatchPid, update),
    timer:send_after(?MODIFIERS, MatchPid, modifiers),
    MatchPid. 

loop(Pids, Projs, Mod, Counter) ->
    receive 
        update -> 
            NewPids = player_movement(Pids),
            NewPids2 = player_collision(NewPids),
            NewProjs = proj_movement(Projs),
            {NewPids3, NewProjs2} = proj_collision(NewPids2, NewProjs),
            {NewPids4, NewMod} = mod_collision(NewPids3, Mod),
            Players = maps:keys(NewPids4),
            [Pid ! {player_pos, Player#player.id, Player#player.p} || Player <- maps:values(NewPids4), Pid <- Players],
            [Pid ! {player_aim, Player#player.id, Player#player.aim} || Player <- maps:values(NewPids4), Pid <- Players],
            [Pid ! {proj_pos, Id, Proj#proj.p} || {Id, Proj} <- lists:enumerate(NewProjs2), Pid <- Players],
            timer:send_after(?TICK, self(), update),
            loop(NewPids3, NewProjs2, NewMod, Counter);
        modifiers ->
            {NewMod, NewCounter} = gen_modifiers(Pids, Mod, Counter),
            timer:send_after(?MODIFIERS, self(), modifiers),
            loop(Pids, Projs, NewMod, NewCounter);
        {Pid, pressed, K} ->
            NewPlayer = pressed(maps:get(Pid, Pids), K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Projs, Mod, Counter);
        {Pid, unpressed, K} ->
            NewPlayer = unpressed(maps:get(Pid, Pids), K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Projs, Mod, Counter);
        {Pid, clicked, X, Y} ->
            {NewPlayer, NewProjs} = shoot(maps:get(Pid, Pids), Projs, X, Y),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, NewProjs, Mod, Counter);
        {Pid, aim, X, Y} ->
            NewPlayer = aim(maps:get(Pid, Pids), X, Y),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Projs, Mod, Counter);
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
