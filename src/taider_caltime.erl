

-module(taider_caltime).

-include("../include/taider.hrl").
-include("../include/caltime.hrl").

-export([
        tai/2,
        utc/2,
        fmt/1,
        scan/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.



-spec tai(#caltime{}, non_neg_integer()) -> #tai{}.

tai(CT, TID) ->
        Day = taider_caldate:mjd(CT#caltime.date),
        %%io:format("tai: mjd: ~B~n", [Day]),

        S0 = CT#caltime.hour * 60 + CT#caltime.minutes,
        S1 = (S0 - CT#caltime.offset) * 60 + CT#caltime.seconds,
        %%io:format("tai: s: ~B~n", [S1]),

        TAI0 = #tai{ x = Day * 86400 + 4611686014920671114 + S1},
        %%io:format("tai: pre-leapsec-add: ~B~n", [TAI0#tai.x]),

        taider_leapsecs:add(TAI0, CT#caltime.seconds =:= 60, TID).
       
       
-spec utc(#tai{}, non_neg_integer()) -> {#caltime{}, non_neg_integer(), non_neg_integer()}.

utc(TAI, TID) ->
        TAI0 = TAI,

        %%io:format("TAI: ~p~n", [TAI0]),
        {TAI1, Leap} = taider_leapsecs:sub(TAI0, TID),
        %%io:format("TAI: ~p~n", [TAI1]),
        U    = TAI1#tai.x,

        U0   = U + 58486,
        S    = U0 rem 86400,

        Sec0 = (S rem 60),
        %%io:format("Sec0: ~p~n", [Sec0]),
        %%io:format("Leap: ~p~n", [Leap]),
        Sec1 = if Leap == false -> Sec0; true  -> Sec0 + 1 end,
        %%io:format("S: ~p~n", [S]),
        S0   = S div 60,

        Min0 =  S0 rem 60,
        S1   =  S0 div 60,

        U1   = U0 div 86400,
        %%io:format("U1: ~p~n", [U0]),
        U2   = U1 - 53375995543064,

        %%io:format("U2: ~p~n", [U2]),
        {CD, PWDay, PYDay} = taider_caldate:frommjd(U2),
        %%io:format("CD: ~p~n", [CD]),

        CT = #caltime{
                date    = CD,
                seconds = Sec1,
                minutes = Min0,
                hour    = S1,
                offset  = 0
        },

        {CT, PWDay, PYDay}.



-spec scan(string()) -> #caltime{}.

scan(Str) ->
        [Date, Time, Offset] = string:tokens(Str, "\s\t"),

        CD = taider_caldate:scan(Date),

        [H,M,S] = string:tokens(Time, ":"),

        {Hour, []} = string:to_integer(H),
        {Mins, []} = string:to_integer(M),
        {Secs, []} = string:to_integer(S),

        {Off, []}  = string:to_integer(Offset),

        #caltime{
                date    = CD,
                hour    = Hour,
                minutes = Mins,
                seconds = Secs,
                offset  = Off
        }.



-spec fmt(#caltime{}) -> string().

fmt(CT) ->
        Date = taider_caldate:fmt(CT#caltime.date),

        Sign = if CT#caltime.offset < 0 -> "";
                                   true -> "+"
               end,

        lists:flatten(io_lib:format("~s ~B:~B:~B ~s~4..0B",
                        [Date, CT#caltime.hour, 
                         CT#caltime.minutes, CT#caltime.seconds, 
                         Sign, CT#caltime.offset])).

