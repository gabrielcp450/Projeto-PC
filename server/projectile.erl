-module(projectile).
-export([
    shoot/4,
    aim/3
]).
-import(utils, [
    sub/2,
    add/2,
    mul/2,
    normalize/1,
    constrain/3
]).

-include("match.hrl").

shoot(Player, Counter, X, Y) ->
    ConstX = constrain(erlang:list_to_float(X), 0, 1),
    ConstY = constrain(erlang:list_to_float(Y), 0, 1),
    Aim = normalize(sub({ConstX, ConstY}, Player#player.p)),
    Projs = Player#player.projs,
    NewProjs = Projs#{Counter => #proj{p = add(Player#player.p, mul(Aim, 0.05)), v = mul(Aim, Player#player.proj_v )}},
    {Player#player{projs = NewProjs, aim = Aim}, Counter+1}.

aim(Player, X, Y) ->
    ConstX = constrain(erlang:list_to_float(X), 0, 1),
    ConstY = constrain(erlang:list_to_float(Y), 0, 1),
    Aim = normalize(sub({ConstX, ConstY}, Player#player.p)),
    Player#player{aim = Aim}.
