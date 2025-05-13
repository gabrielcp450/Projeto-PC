-module(utils).
-export([
    sub/2,
    add/2,
    mul/2,
    normalize/1,
    constrain/3
]).

sub({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

mul({X1, Y1}, C) ->
    {X1 * C, Y1 * C}.

normalize({X, Y}) ->
    Length = math:sqrt(X*X + Y*Y),
    case Length > 0 of
        true -> {X/Length, Y/Length};
        false -> {0, 0}
    end.

constrain(Value, Min, Max) ->
    case Value < Min of
        true -> Min;
        false -> case Value > Max of
            true -> Max;
            false -> Value
        end
    end.
