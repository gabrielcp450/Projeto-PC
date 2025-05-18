-module(projectile).
-export([
    shoot/2,
    reload_or_shoot/3,
    aim/3,
    get_all_projectiles/1
]).
-import(utils, [
    sub/2,
    add/2,
    mul/2,
    normalize/1,
    constrain/3
]).

-include("match.hrl").

shoot(Player, Counter) ->
    Projs = Player#player.projs,
    Aim = Player#player.aim,
    NewProjs = Projs#{Counter => #proj{p = add(Player#player.p, mul(Aim, 0.05)), v = mul(Aim, Player#player.proj_v)}},
    {Player#player{projs = NewProjs, aim = Aim}, Counter+1}.

reload_or_shoot(Player, Pid, Counter) ->
    case Player#player.reloading of
        false ->
            NewPlayer = Player#player{reloading = true},
            timer:send_after(round(NewPlayer#player.proj_i * 1000), self(), {reloaded, Pid}),
            shoot(NewPlayer, Counter);
        true -> {Player, Counter}
    end.

aim(Player, X, Y) ->
    ConstX = constrain(erlang:list_to_float(X), 0, 1),
    ConstY = constrain(erlang:list_to_float(Y), 0, 1),
    Aim = normalize(sub({ConstX, ConstY}, Player#player.p)),
    Player#player{aim = Aim}.

get_all_projectiles(Pids) ->
    [{_, Player1}, {_, Player2}] = maps:to_list(Pids),
    maps:to_list(maps:merge(Player1#player.projs, Player2#player.projs)).