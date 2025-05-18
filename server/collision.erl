-module(collision).
-export([
    player_collision/1,
    proj_collision/1,
    mod_collision/2,
    collides_sphere_to_sphere/4,
    collides_sphere_to_wall/2,
    get_players/1
]).
-import(match, [
    initial_pos/1
]).

-include("match.hrl").

collides_sphere_to_wall({X, Y}, R) ->
    X - R =< 0 orelse X + R >= 1 orelse Y - R =< 0 orelse Y + R >= 1.

collides_sphere_to_sphere({X1, Y1}, R1, {X2, Y2}, R2) ->
    Dist = math:sqrt((X1 - X2)*(X1 - X2) + (Y1 - Y2)*(Y1 - Y2)),
    Dist =< R1 + R2.

handle_player_hit_wall(FP, FPid, SP, SPid, HitPlayer, PointsInc) ->
    FPPos = (initial_pos(0))#player.p,
    SPPos = (initial_pos(1))#player.p,
    case HitPlayer of
        first ->
            FPNew = FP#player{p = FPPos, a = {0, 0}, v = {0, 0}, k = #keys{}},
            SPNew = SP#player{p = SPPos, a = {0, 0}, v = {0, 0}, k = #keys{}, points = SP#player.points + PointsInc};
        second ->
            SPNew = SP#player{p = SPPos, a = {0, 0}, v = {0, 0}, k = #keys{}},
            FPNew = FP#player{p = FPPos, a = {0, 0}, v = {0, 0}, k = #keys{}, points = FP#player.points + PointsInc}
    end,
    FPid ! {score, FPNew#player.points, SPNew#player.points},
    SPid ! {score, SPNew#player.points, FPNew#player.points},
    {hit, #{FPid => FPNew, SPid => SPNew}}.

handle_player_hit(FP, FPid, SP, SPid, HitPlayer, PointsInc) ->
    {FPNew, SPNew} = case HitPlayer of
        first ->
            {FP, SP#player{points = SP#player.points + PointsInc}};
        second ->
            {FP#player{points = FP#player.points + PointsInc}, SP}
    end,
    FPid ! {score, FPNew#player.points, SPNew#player.points},
    SPid ! {score, SPNew#player.points, FPNew#player.points},
    #{FPid => FPNew, SPid => SPNew}.

get_players(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),
    case Player1#player.id of
        0 -> {Player1, Pid1, Player2, Pid2};
        1 -> {Player2, Pid2, Player1, Pid1}
    end.

check_proj_hit_players(Projs, FP, FPid, SP, SPid) ->
    maps:fold(fun(Id, Proj, {PidsAcc, ToRemove}) ->
        {X, Y} = Proj#proj.p,
        case collides_sphere_to_sphere({X, Y}, ?PROJECTILE_RADIUS, FP#player.p, ?PLAYER_RADIUS) of
            true ->
                UpdatedPids = handle_player_hit(FP, FPid, SP, SPid, first, 1),
                {maps:merge(PidsAcc, UpdatedPids), [Id | ToRemove]};
            false ->
                case collides_sphere_to_sphere({X, Y}, ?PROJECTILE_RADIUS, SP#player.p, ?PLAYER_RADIUS) of
                    true ->
                        UpdatedPids = handle_player_hit(FP, FPid, SP, SPid, second, 1),
                        {maps:merge(PidsAcc, UpdatedPids), [Id | ToRemove]};
                    false ->
                        {PidsAcc, ToRemove}
                end
        end
    end, ({#{FPid => FP, SPid => SP}, []}), Projs).

player_collision(Pids) ->
    {FP, FPid, SP, SPid} = get_players(Pids),
    case collides_sphere_to_wall(FP#player.p, ?PLAYER_RADIUS) of
        true -> handle_player_hit_wall(FP, FPid, SP, SPid, first, 2);
        false ->
            case collides_sphere_to_wall(SP#player.p, ?PLAYER_RADIUS) of
                true -> handle_player_hit_wall(FP, FPid, SP, SPid, second, 2);
                false -> {didnt_hit,Pids}
            end
    end.

proj_collision(Pids) ->
    {FP, FPid, SP, SPid} = get_players(Pids),

    Projs = maps:merge(FP#player.projs, SP#player.projs),
    {NewPids, HitIds} = check_proj_hit_players(Projs, FP, FPid, SP, SPid),

    % Notify clients about removed projectiles
    [Pid ! {proj_rem, Id} || Pid <- maps:keys(Pids), Id <- HitIds],

    % Filter out projectiles that hit players
    {NewFP, FPid, NewSP, SPid} = get_players(NewPids),
    NewFP2 = NewFP#player{projs = maps:without(HitIds, NewFP#player.projs)},
    NewSP2 = NewSP#player{projs = maps:without(HitIds, NewSP#player.projs)},

    % Then check for wall collisions
    RemainingProjs = maps:merge(NewFP2#player.projs, NewSP2#player.projs),
    FinalProjs = maps:filter(fun(Id, Proj) ->
        {X, Y} = Proj#proj.p,
        case collides_sphere_to_wall({X, Y}, ?PROJECTILE_RADIUS) of
            true ->
                [Pid ! {proj_rem, Id} || Pid <- maps:keys(Pids)],
                false;
            false -> true
        end
    end, RemainingProjs),

    % Filter out projectiles that hit walls
    NewFP3 = NewFP2#player{projs = maps:with(maps:keys(FinalProjs), NewFP2#player.projs)},
    NewSP3 = NewSP2#player{projs = maps:with(maps:keys(FinalProjs), NewSP2#player.projs)},
    
    #{FPid => NewFP3, SPid => NewSP3}.

buff_player(Player, Id) ->
    case Id of 
        0 -> Player#player{proj_v = ?PROJECTILE_VELOCITY_MAX};
        1 -> Player#player{proj_v = ?PROJECTILE_VELOCITY_MIN};
        2 -> Player#player{proj_i = ?PROJECTILE_INTERVAL_MIN};
        3 -> Player#player{proj_i = ?PROJECTILE_INTERVAL_MAX};
        _ -> Player
    end.

handle_player_buff(FP, FPid, SP, SPid, BuffPlayer, Id) ->
    {FPNew, SPNew} = case  BuffPlayer of
        first ->
            {buff_player(FP, Id), SP};
        second ->
            {FP, buff_player(SP, Id)}
    end,
    #{FPid => FPNew, SPid => SPNew}.

unfold_mod(Mod) ->
    [{Type, {Id, Pos}} || {Type, L} <- maps:to_list(Mod), {Id,Pos} <- L].

check_modifier_hit_players(Mod, FP, FPid, SP, SPid) ->
    lists:foldl(fun({Type, {Id, Pos}}, {PidsAcc, ToRemove}) ->
        case collides_sphere_to_sphere(Pos, ?MODIFIER_RADIUS, FP#player.p, ?PLAYER_RADIUS) of
            true ->
                UpdatedPids = handle_player_buff(FP, FPid, SP, SPid, first, Type),
                {maps:merge(PidsAcc, UpdatedPids), [{Type, {Id,Pos}} | ToRemove]};
            false ->
                case collides_sphere_to_sphere(Pos, ?MODIFIER_RADIUS, SP#player.p, ?PLAYER_RADIUS) of
                true ->
                    UpdatedPids = handle_player_buff(FP, FPid, SP, SPid, second, Type),
                    {maps:merge(PidsAcc, UpdatedPids), [{Type, {Id,Pos}} | ToRemove]};
                false ->
                    {PidsAcc, ToRemove}
                end 
            end 
        end, ({#{FPid => FP, SPid => SP}, []}), unfold_mod(Mod)).

remove_mod(Mod, ToRemove) ->
    lists:foldl(
        fun({Id, Element}, AccMap) ->
            maps:update_with(Id,
                fun(List) -> lists:filter(fun(E) -> E =/= Element end, List) end,
                AccMap
            )
        end,
        Mod,
        ToRemove 
    ).

mod_collision(Pids, Mod) ->

    {FP, FPid, SP, SPid} = get_players(Pids),

    {NewPids, ToRemove} = check_modifier_hit_players(Mod, FP, FPid, SP, SPid),

    [Pid ! {modifier_rem, Id} || Pid <- maps:keys(Pids), {_, {Id, _}} <- ToRemove],

    NewMod = remove_mod(Mod, ToRemove),

    {NewPids, NewMod}.

