-module(modifiers).
-export([
    gen_modifiers/2
]).

-include("match.hrl").

gen_random() ->
    rand:uniform(99)/100.

gen_modifiers(Pids,Mod) ->
    L = [M || {M, X} <- maps:to_list(Mod), length(X) < ?MAX_MODIFIERS],
    case L of 
        [] ->
            Mod;
        _ ->
            Nt = rand:uniform(length(L)),
            Choosen = lists:nth(Nt, L),
            C = {gen_random(), gen_random()},
            [Pid ! {modifier_pos, Choosen, C} || Pid <- maps:keys(Pids)],
            S = maps:get(Choosen, Mod),
            maps:update(Choosen, S ++ [C], Mod)
    end.
