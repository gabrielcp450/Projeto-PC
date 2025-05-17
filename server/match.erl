-module(match).
-export([create/2, initial_pos/1]).
-import(movement, [
    player_movement/1,
    proj_movement/1
]).
-import(collision, [
    player_collision/1,
    proj_collision/1,
    mod_collision/2,
    get_players/1
]).
-import(projectile, [
    shoot/2,
    reload_or_shoot/3,
    aim/3,
    get_all_projectiles/1
]).
-import(keys, [
    pressed/2,
    unpressed/2
]).
-import(modifiers, [
    gen_modifiers/3,
    tick_buff_pids/1
]).

-include("match.hrl").

initial_pos(I) ->
    Px = 1/2 * (I+1) - 1/4,
    #player{p = {Px, 1/2}, id = I}.

create(Pid1, Pid2) -> 
    Pids = #{Pid1 => initial_pos(0), Pid2 => initial_pos(1)},
    Mod = # {0 => [], 1 => [], 2 => [], 3 => []},
    MatchPid = spawn(fun() -> loop(Pids, Mod, 0, erlang:monotonic_time(millisecond)) end),
    timer:send_after(2*60*1000, MatchPid, finished),
    timer:send_interval(?TICK, MatchPid, update),
    timer:send_interval(?MODIFIERS_INTERVAL, MatchPid, modifiers),
    MatchPid.

set_results(Pids) ->
    {FP, FPid, SP, SPid} = get_players(Pids),
    Points1 = FP#player.points,
    Points2 = SP#player.points,
    {Result1, Result2} = if 
        Points1 < Points2 -> 
            {-1, 1};
        Points1 > Points2 ->
            {1, -1};
        true ->
            {0, 0}
    end,
    FPid ! {finished, Result1},
    SPid ! {finished, Result2}.

loop(Pids, Mod, Counter, PrevTime) ->
    receive 
        update -> 
            NewPids = tick_buff_pids(Pids),
            NewPids1 = player_movement(NewPids),
            NewPids2 = player_collision(NewPids1),
            NewPids3 = proj_movement(NewPids2),
            NewPids4 = proj_collision(NewPids3),
            {NewPids5, NewMod} = mod_collision(NewPids4, Mod),
            Players = maps:keys(NewPids5),
            [Pid ! {player_pos, P#player.id, P#player.p} || P <- maps:values(NewPids5), Pid <- Players],
            [Pid ! {player_aim, P#player.id, P#player.aim} || P <- maps:values(NewPids5), Pid <- Players],
            [Pid ! {proj_pos, Id, P#proj.p} || {Id, P} <- get_all_projectiles(NewPids5), Pid <- Players],
            % io:format("[Match ~p] Dt: ~pms~n", [self(), erlang:monotonic_time(millisecond)-PrevTime]),
            loop(NewPids5, NewMod, Counter, erlang:monotonic_time(millisecond));
        modifiers ->
            {NewMod, NewCounter} = gen_modifiers(Pids, Mod, Counter),
            loop(Pids, NewMod, NewCounter, PrevTime);
        {reloaded, Pid} ->
            Player = maps:get(Pid, Pids),
            NewPlayer = Player#player{reloading = false},
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter, PrevTime);
        {Pid, pressed, K} ->
            NewPlayer = pressed(maps:get(Pid, Pids), K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter, PrevTime);
        {Pid, unpressed, K} ->
            NewPlayer = unpressed(maps:get(Pid, Pids), K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter, PrevTime);
        {Pid, clicked, X, Y} ->
            NewPlayer = aim(maps:get(Pid, Pids), X, Y),
            {NewPlayer2, NewCounter} = reload_or_shoot(NewPlayer, Pid, Counter),
            NewPids = maps:update(Pid, NewPlayer2, Pids),
            loop(NewPids, Mod, NewCounter, PrevTime);
        {Pid, aim, X, Y} ->
            NewPlayer = aim(maps:get(Pid, Pids), X, Y),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter, PrevTime);
        finished -> 
            set_results(Pids);
        {Pid, exit} -> 
            [{WPid, _}] = maps:to_list(maps:remove(Pid, Pids)),
            WPid ! {finished, 1},
            Pid ! {finished, -1}
    end.
