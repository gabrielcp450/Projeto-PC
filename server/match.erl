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
    aim/3
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
    MatchPid = spawn(fun() -> loop(Pids, Mod, 0) end),
    timer:send_after(5000000, MatchPid, finished),
    timer:send_after(?TICK, MatchPid, update),
    timer:send_after(?MODIFIERS_INTERVAL, MatchPid, modifiers),
    MatchPid. 

player_shoot_limited(Player, Pid, Counter) ->
    erlang:display("clicked"),
    erlang:display(Player#player.reloading),
    case Player#player.reloading of
        false ->
            NewPlayer = Player#player{reloading = true},
            timer:send_after(round(NewPlayer#player.proj_i * 1000), self(), {reloaded, Pid}),
            shoot(NewPlayer, Counter);
        true -> {Player, Counter}
    end.

loop(Pids, Mod, Counter) ->
    receive 
        update -> 
            NewPids = tick_buff_pids(Pids),
            NewPids1 = player_movement(NewPids),
            NewPids2 = player_collision(NewPids1),
            NewPids3 = proj_movement(NewPids2),
            NewPids4 = proj_collision(NewPids3),
            {NewPids5, NewMod} = mod_collision(NewPids4, Mod),
            Players = maps:keys(NewPids5),
            [Pid ! {player_pos, Player#player.id, Player#player.p} || Player <- maps:values(NewPids5), Pid <- Players],
            [Pid ! {player_aim, Player#player.id, Player#player.aim} || Player <- maps:values(NewPids5), Pid <- Players],
            {FP, _, SP, _} = get_players(NewPids4),
            [Pid ! {proj_pos, Id, Proj#proj.p} || {Id, Proj} <- maps:to_list(maps:merge(FP#player.projs, SP#player.projs)), Pid <- Players],
            timer:send_after(?TICK, self(), update),
            loop(NewPids5, NewMod, Counter);
        modifiers ->
            {NewMod, NewCounter} = gen_modifiers(Pids, Mod, Counter),
            timer:send_after(?MODIFIERS_INTERVAL, self(), modifiers),
            loop(Pids, NewMod, NewCounter);
        {reloaded, Pid} ->
            Player = maps:get(Pid, Pids),
            NewPlayer = Player#player{reloading = false},
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter);
        {Pid, pressed, K} ->
            NewPlayer = pressed(maps:get(Pid, Pids), K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter);
        {Pid, unpressed, K} ->
            NewPlayer = unpressed(maps:get(Pid, Pids), K),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter);
        {Pid, clicked, X, Y} ->
            NewPlayer = aim(maps:get(Pid, Pids), X, Y),
            {NewPlayer2, NewCounter} = player_shoot_limited(NewPlayer, Pid, Counter),
            NewPids = maps:update(Pid, NewPlayer2, Pids),
            loop(NewPids, Mod, NewCounter);
        {Pid, aim, X, Y} ->
            NewPlayer = aim(maps:get(Pid, Pids), X, Y),
            NewPids = maps:update(Pid, NewPlayer, Pids),
            loop(NewPids, Mod, Counter);
        finished ->
            [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),

            Points1 = Player1#player.points,
            Points2 = Player2#player.points,
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
