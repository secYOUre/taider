

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

        S0 = CT#caltime.hour * 60 + CT#caltime.minutes,
        S1 = (S0 - CT#caltime.offset) * 60 + CT#caltime.seconds,

        TAI0 = #tai{ x = Day * 86400 + 4611686014920671114 + S1},

        taider_leapsecs:add(TAI0, CT#caltime.seconds =:= 60, TID).
       
       
-spec utc(#tai{}, non_neg_integer()) -> {#caltime{}, non_neg_integer(), non_neg_integer()}.

utc(TAI, TID) ->
        TAI0 = TAI,

        {TAI1, Leap} = taider_leapsecs:sub(TAI0, TID),
        U    = TAI1#tai.x,

        U0   = U + 58486,
        S    = U0 rem 86400,

        Sec0 = (S rem 60),
        Sec1 = if Leap == false -> Sec0; true  -> Sec0 + 1 end,
        S0   = S div 60,

        Min0 =  S0 rem 60,
        S1   =  S0 div 60,

        U1   = U0 div 86400,
        U2   = U1 - 53375995543064,

        {CD, PWDay, PYDay} = taider_caldate:frommjd(U2),

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

        lists:flatten(io_lib:format("~s ~2..0B:~2..0B:~2..0B ~s~4..0B",
                        [Date, CT#caltime.hour, 
                         CT#caltime.minutes, CT#caltime.seconds, 
                         Sign, CT#caltime.offset])).

