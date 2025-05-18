-module(stats).

-export([start/0, insert/1, get_level/1, add_win/1, add_loss/1, top10/0, stop/0]).

-record(data, {level = 1, win = 0, loss = 0, win_streak = 0, lose_streak = 0}).

insert(User) ->
    rpc({insert, User}).

get_level(User) ->
    rpc({level, User}).

add_win(User) ->
    rpc({win, User}).

add_loss(User) ->
    rpc({loss, User}).

top10() ->
    rpc({top}).

stop() ->
    rpc({stop}).

save(Map) ->
    Bin = term_to_binary(Map),
    file:write_file("storage/stats.bin", Bin).

%aux({U1, L1, W1, Lo1}, {U2, L2, W2, Lo2}) ->
%    case {L1, W1, -Lo1, U1} =< {L2, W2, -Lo2, U2} of
%        true -> true;
%        false -> false
%    end.

aux({U1, L1, W1, Lo1}, {U2, L2, W2, Lo2}) -> 
    if 
        L1 > L2 ->
            true;
        L1 < L2 ->
            false;
        W1 > W2 ->
            true;
        W1 < W2 ->
            false;
        Lo1 < Lo2 ->
            true;
        Lo1 > Lo2 ->
            false;
        true ->
            U1 =< U2
    end.

start() ->
    case file:read_file("storage/stats.bin") of 
        {ok, Bin} -> 
            Map = binary_to_term(Bin);
        {error, _ } ->
            Map = #{}
    end,
    Pid = spawn(fun() -> stats(Map) end),
    register(?MODULE, Pid).

stats(Map) ->
    receive
        {Pid, {insert, User}} ->
            NewMap = maps:put(User, #data{}, Map),
            save(NewMap),
            Pid ! {insert},
            stats(NewMap);
        {Pid, {level, User}} ->
            Data = maps:get(User, Map),
            Pid ! {level, Data#data.level},
            stats(Map);
        {Pid, {win, User}} ->
            Data = maps:get(User, Map),
            Level = Data#data.level,    
            Win = Data#data.win,    
            WinS = Data#data.win_streak,
            Msg = if 
                Level == WinS + 1 ->
                    NewMap = maps:update(User, Data#data{level = Level + 1, win = Win + 1, win_streak = 0, lose_streak = 0}, Map),
                    level_up;
                true ->
                    NewMap = maps:update(User, Data#data{win = Win + 1, win_streak = WinS + 1, lose_streak = 0}, Map),
                    ok
            end,
            Pid ! {win, Msg},
            save(NewMap),
            stats(NewMap);
        {Pid, {loss, User}} ->
            Data = maps:get(User, Map),
            Level = Data#data.level,    
            Loss = Data#data.loss,    
            LossS = Data#data.lose_streak,
            Msg = if 
                Level == 1 ->
                    NewMap = maps:update(User, Data#data{loss = Loss + 1, lose_streak = LossS + 1, win_streak = 0}, Map),
                    ok;
                (Level + 1) div 2 == LossS + 1 ->
                    NewMap = maps:update(User, Data#data{level = Level - 1, loss = Loss + 1, lose_streak = 0, win_streak = 0}, Map),
                    level_down;
                true ->
                    NewMap = maps:update(User, Data#data{loss = Loss + 1, lose_streak = LossS + 1, win_streak = 0}, Map),
                    ok
            end,
            Pid ! {loss, Msg},
            save(NewMap),
            stats(NewMap);

        {Pid, {top}} ->
            L = [{User, Level, WinS, LossS} || {User, #data{level = Level, win_streak = WinS, lose_streak = LossS}} <- maps:to_list(Map)],
            L1 = lists:sort(fun aux/2,L),
            Top10 = lists:sublist(L1, 10),
            Pid ! {top, Top10}, 
            stats(Map);
        {Pid, {stop}} ->
            save(Map),
            Pid ! {stop}
    end.

rpc(Request) ->
    ?MODULE ! {self(), Request},
    Tag = case Request of
        {T} -> T;
        {T, _} -> T
    end,
    receive
        {Tag} -> true;
        {Tag, Msg} -> Msg
    end.
