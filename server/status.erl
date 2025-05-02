-module(status).

-export([start/0, insert/1, get_level/1, addWin/1, addLoss/1, top10/0, save/0, stop/0]).

-record(data, {level = 1, win = 0, loss = 0, win_streak = 0, lose_streak = 0}).



start() ->
    case file:read_file("storage/status.bin") of 
        {ok, Bin} -> 
            Map = binary_to_term(Bin);
        {error, _ } ->
            Map = #{}
    end,
    Pid = spawn(fun() -> status(Map) end),
    register(?MODULE, Pid).
insert(User) ->
    erlang:display("hello"),
    ?MODULE ! {self(), insert, User},
    receive Msg -> Msg end.

% assume that the user exists
get_level(User) ->
    ?MODULE ! {self(), level, User},
    receive Msg -> Msg end.

addWin(User) ->
    ?MODULE ! {self(), win, User},
    receive Msg -> Msg end.

addLoss(User) ->
    ?MODULE ! {self(), loss, User},
    receive Msg -> Msg end.
top10() ->
    ?MODULE ! {self(), top},
    receive Msg -> Msg end.

save() ->
    ?MODULE ! save.
stop() ->
    ?MODULE ! stop.

%aux({U1, L1, W1, Lo1}, {U2, L2, W2, Lo2}) ->
%    case {L1, W1, -Lo1, U1} =< {L2, W2, -Lo2, U2} of
%        true -> true;
%        false -> false
%    end.


aux({U1, L1, W1, Lo1}, {U2, L2, W2, Lo2}) -> 
    if 
        L1 > L2 ->
            true;
        L2 < L1 ->
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


status(Map) ->
    receive
        {Pid, insert, User} ->
            NewMap = maps:put(User, #data{}, Map),
            Pid ! ok,
            status(NewMap);
        {Pid, level, User} ->
            Data = maps:get(User, Map),
            Pid ! Data#data.level,
            status(Map);
        {Pid, win, User} ->
            Data = maps:get(User, Map),
            Level = Data#data.level,    
            Win = Data#data.win,    
            WinS = Data#data.win_streak,
            if 
                Level == WinS ->
                    NewMap = maps:update(User, Data#data{level = Level + 1, win = Win +1,win_streak = WinS + 1, lose_streak = 0}, Map),
                    Pid ! level_up;
                true ->
                    NewMap = maps:update(User, Data#data{win = Win + 1, win_streak = WinS + 1, lose_streak = 0}),
                    Pid ! ok
            end,
            status(NewMap);
        {Pid, loss, User} ->
            Data = maps:get(User, Map),
            Level = Data#data.level,    
            Loss = Data#data.loss,    
            LossS = Data#data.lose_streak,
            if 
                Level == 1 ->
                    NewMap = maps:upate(User, Data#data{loss = Loss + 1,lose_streak = LossS + 1, win_streak = 0}),
                    Pid ! ok;
                Level + 2 div 2 == LossS ->
                    NewMap = maps:update(User, Data#data{level = Level - 1, loss = Loss +1,lose_streak = 0 , win_streak = 0}, Map),
                    Pid ! level_down;
                true ->
                    NewMap = maps:update(User, Data#data{loss = Loss + 1, lose_streak = LossS + 1, win_streak = 0}, Map),
                    Pid ! ok
            end,
            status(NewMap);

        {Pid, top} ->
            L = [{User, Level, WinS, LossS} || {User, #data{level = Level, win_streak = WinS, loss = LossS}} <- maps:to_list(Map)],
            L1 = lists:sort(fun aux/2,L),
            Top10 =lists:sublist(L1, 10),
            Pid ! Top10, 
            status(Map);
        save ->
            Bin = term_to_binary(Map),
            file:write_file("storage/status.bin", Bin),
            status(Map);
        stop->
            Bin = term_to_binary(Map),
            file:write_file("storage/status.bin", Bin)

    end.