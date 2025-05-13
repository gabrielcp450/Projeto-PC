-module(keys).
-export([
    pressed/2,
    unpressed/2
]).

-include("match.hrl").

pressed(Player, K) ->
    Keys = Player#player.k,
    NewKeys = case K of
        "w" -> Keys#keys{w = true};
        "s" -> Keys#keys{s = true};
        "a" -> Keys#keys{a = true};
        "d" -> Keys#keys{d = true};
        _ -> Keys
    end,
    Player#player{k = NewKeys}.

unpressed(Player, K) ->
    Keys = Player#player.k,
    NewKeys = case K of
        "w" -> Keys#keys{w = false};
        "s" -> Keys#keys{s = false};
        "a" -> Keys#keys{a = false};
        "d" -> Keys#keys{d = false};
        _ -> Keys
        end,
    Player#player{k = NewKeys}.
