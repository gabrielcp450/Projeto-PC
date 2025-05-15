-module(modifiers).
-export([
    gen_modifiers/3,
    tick_buff_pids/1
]).

-import(collision,[
    collides_sphere_to_wall/2,
    collides_sphere_to_sphere/4,
    get_players/1
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

tick_buff(Player) ->
    Epsilon = 0.00001,
    Proj_v = Player#player.proj_v,
    Proj_i = Player#player.proj_i,
    NewProj_v = 
        if Proj_v < ?PROJECTILE_VELOCITY_INITIAL - Epsilon ->
                Proj_v + ?VELOCITY_TICK_BUFF;
            Proj_v > ?PROJECTILE_VELOCITY_INITIAL + Epsilon ->
                Proj_v - ?VELOCITY_TICK_BUFF;
            true -> ?PROJECTILE_VELOCITY_INITIAL
        end,
    NewProj_i = 
        if Proj_i < ?PROJECTILE_INTERVAL_INITIAL - Epsilon ->
                Proj_i + ?INTERVAL_TICK_BUFF;
            Proj_i > ?PROJECTILE_INTERVAL_INITIAL + Epsilon ->
                Proj_i - ?INTERVAL_TICK_BUFF;
            true -> ?PROJECTILE_INTERVAL_INITIAL
        end,

    Player#player{proj_v = NewProj_v, proj_i = NewProj_i}.

tick_buff_pids(Pids) ->
    {FP, FPid, SP, SPid} = get_players(Pids),

    NewFP = tick_buff(FP),
    NewSP = tick_buff(SP),
    #{FPid => NewFP,SPid => NewSP}.
