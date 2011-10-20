

-module(taider_utils).

-include("../include/taider.hrl").
-include("../include/caltime.hrl").

-export([
        nowutc/0,
        easter/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.



-spec nowutc() -> string().

nowutc() ->
        {ok, Id} = taider_leapsecs:init(),

        Now = taider_taia:now(),
        X   = taider_taia:fmtfrac(Now),
        Sec = taider_taia:tai(Now),

        {CT,_,_}  = taider_caltime:utc(Sec, Id),
        CD        = CT#caltime.date,

        Result = lists:flatten(
                   io_lib:format("~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~s",
                        [CD#caldate.year,
                         CD#caldate.month,
                         CD#caldate.day,
                         CT#caltime.hour,
                         CT#caltime.minutes,
                         CT#caltime.seconds,
                         X])),
        Result.

-spec easter(non_neg_integer()) -> string().

easter(Year) ->
        %% Day names listed for the sake of software testing.
        %% Easter is on Sunday, no matter the year.
        DayName = [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ],

        CD  = taider_caldate:easter({caldate, Year, 0, 0}),
        Day = taider_caldate:mjd(CD),

        {CD0, WeekDay, YearDay} = taider_caldate:frommjd(Day),
        "Sun" = lists:nth(WeekDay + 1, DayName),
        Out = taider_caldate:fmt(CD0),

        Result = lists:flatten(io_lib:format("~s ~s yearday ~B  mjd ~B", 
                [lists:nth(WeekDay + 1, DayName),
                 Out, YearDay, Day])),
                 
        Result.
