-module(collision).
-export([
    player_collision/1,
    proj_collision/2
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

handle_player_hit(FP, FPid, SP, SPid, HitPlayer, PointsInc) ->
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
    #{FPid => FPNew, SPid => SPNew}.

get_players(Pids) ->
    [{Pid1, Player1}, {Pid2, Player2}] = maps:to_list(Pids),
    case Player1#player.id of
        0 -> {Player1, Pid1, Player2, Pid2};
        1 -> {Player2, Pid2, Player1, Pid1}
    end.

check_proj_hit_players(Projs, FP, FPid, SP, SPid) ->
    lists:foldl(fun(Proj, Acc) ->
        case Acc of
            {hit, _} -> Acc;
            none ->
                {X, Y} = Proj#proj.p,
                case collides_sphere_to_sphere({X, Y}, ?PROJECTILE_RADIUS, FP#player.p, ?PLAYER_RADIUS) of
                    true ->
                        {hit, handle_player_hit(FP, FPid, SP, SPid, first, 1)};
                    false ->
                        case collides_sphere_to_sphere({X, Y}, ?PROJECTILE_RADIUS, SP#player.p, ?PLAYER_RADIUS) of
                            true -> {hit, handle_player_hit(FP, FPid, SP, SPid, second, 1)};
                            false -> none
                        end
                end
        end
    end, none, Projs).

player_collision(Pids) ->
    {FP, FPid, SP, SPid} = get_players(Pids),
    case collides_sphere_to_wall(FP#player.p, ?PLAYER_RADIUS) of
        true -> handle_player_hit(FP, FPid, SP, SPid, first, 2);
        false ->
            case collides_sphere_to_wall(SP#player.p, ?PLAYER_RADIUS) of
                true -> handle_player_hit(FP, FPid, SP, SPid, second, 2);
                false -> Pids
            end
    end.

proj_collision(Pids, Projs) ->
    {FP, FPid, SP, SPid} = get_players(Pids),
    case check_proj_hit_players(Projs, FP, FPid, SP, SPid) of
        {hit, NewPids} ->
            [Pid ! {proj_rem, Id} || Pid <- maps:keys(Pids), {Id, _} <- lists:enumerate(Projs)],
            {NewPids, []};
        none ->
            Projs2 = lists:filter(fun({Id, Proj}) ->
                {X, Y} = Proj#proj.p,
                case collides_sphere_to_wall({X, Y}, ?PROJECTILE_RADIUS) of
                    true ->
                        [Pid ! {proj_rem, Id} || Pid <- maps:keys(Pids)],
                        false;
                    false -> true
                end
            end, lists:enumerate(Projs)),
            {Pids, lists:map(fun({_, Proj}) -> Proj end, Projs2)}
    end.
