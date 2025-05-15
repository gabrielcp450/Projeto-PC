-module(modifiers).
-export([
    gen_modifiers/3
]).

-import(collision,[
    collides_sphere_to_wall/2,
    collides_sphere_to_sphere/4
]).
-include("match.hrl").
gen_random(L) ->
    Pos = {rand:uniform(99)/100, rand:uniform(99)/100},
    Pred = fun(E) -> collides_sphere_to_sphere(Pos, ?MODIFIER_RADIUS, E, ?MODIFIER_RADIUS) end,
    
    CollideWithWall = collides_sphere_to_wall(Pos, ?MODIFIER_RADIUS),
    CollideWithAny = lists:any(Pred, L),
    
    case CollideWithWall orelse CollideWithAny of
        true -> gen_random(L);
        false -> Pos
    end.

gen_modifiers(Pids,Mod, Counter) ->
    Types = [M || {M, X} <- maps:to_list(Mod), length(X) < ?MAX_MODIFIERS],
    L = [Pos || {_, Pos} <- maps:values(Mod)],

    case Types of 
        [] ->
            {Mod, Counter};
        _ ->
            Nt = rand:uniform(length(Types)),
            Type = lists:nth(Nt, Types),
            P = gen_random(L),
            [Pid ! {modifier_pos, Counter, Type, P} || Pid <- maps:keys(Pids)],
            S = maps:get(Type, Mod),
            {maps:update(Type, S ++ [{Counter,P}], Mod), Counter + 1}
    end.
