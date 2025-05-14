-module(modifiers).
-export([
    gen_modifiers/3
]).

-include("match.hrl").

gen_random() ->
    rand:uniform(99)/100.

gen_modifiers(Pids,Mod, Counter) ->
    L = [M || {M, X} <- maps:to_list(Mod), length(X) < ?MAX_MODIFIERS],
    case L of 
        [] ->
            {Mod, Counter};
        _ ->
            Nt = rand:uniform(length(L)),
            Type = lists:nth(Nt, L),
            C = {gen_random(), gen_random()},
            [Pid ! {modifier_pos, Counter, Type, C} || Pid <- maps:keys(Pids)],
            S = maps:get(Type, Mod),
            {maps:update(Type, S ++ [{Counter,C}], Mod), Counter + 1}
    end.
