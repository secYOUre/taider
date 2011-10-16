

-module(taider_caldate).

-include("../include/caldate.hrl").

-export([
        mjd/1,
        frommjd/1,
        scan/1,
        fmt/1,
        normalize/1,
        easter/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.



-spec mjd(#caldate{}) -> non_neg_integer().

mjd(CD) ->
        Day   = CD#caldate.day   - 678882,
        Month = CD#caldate.month - 1,
        Year  = CD#caldate.year,

        Day0  = Day + (146097 * (Year div 400)),
        Year0 = Year rem 400,

        {Month0, Year1} = if Month >= 2 ->
                {Month - 2, Year0};
           true       ->
                {Month + 10, Year0 - 1}
        end,

        Year2  = Year1 + (Month0 div 12),
        Month1 = Month0 rem 12,
        {Month2, Year3} = if Month1 < 0 ->
                {Month1 + 12, Year2 - 1};
           true       ->
                {Month1, Year2}
        end,
        
        Day1  = Day0 + lists:nth(Month2+1, ?MONTHTAB),

        Day2  = Day1 + (146097 * (Year3 div 400)),
        Year4 = Year3 rem 400,
        {Year5, Day3} = if Year4 < 0 ->
                {Year4 + 400, Day2 - 146097};
           true      ->
                {Year4, Day2}
        end,        

        Day4  = Day3 + lists:nth((Year5 band 3) + 1, ?TIMES365),
        Year6 = Year5 bsr 2,

        Day5  = Day4 + (1461 * (Year6 rem 25)),
        Year7 = Year6 div 25,

        Day6  = Day5 + lists:nth((Year7 band 3) + 1, ?TIMES36524),

        Day6.


-spec frommjd(integer()) -> {#caldate{}, integer(), integer()}.

frommjd(Day) ->
        Year = Day div 146097,
        Day0 = Day rem 146097,
        Day1 = Day0 + 678881,

        {Day2, Year0} = dyupdate(Day1, Year),

        %% year * 146097 + day - 768881 is MJD; 0 =< day < 146097
        %% 2000-03-01, MJD 51604, is year 5, day 0

        PwDay = (Day2 + 3) rem 7,

        %% io:format("PwDay: ~B, Day: ~B, Year: ~B~n", [PwDay, Day2, Year0]),

        Year1 = Year0 * 4,
        {Year2, Day3} = if Day2 =:= 146096 ->
                {Year1 + 3, 36524};
            true ->
                {Year1 + (Day2 div 36524), Day2 rem 36524}
        end,
        %%io:format("PwDay: ~B, Day: ~B, Year: ~B~n", [PwDay, Day2, Year2]),
        Year3 = Year2 * 25,
        Year4 = Year3 + (Day3 div 1461),
        Day4  = Day3 rem 1461,
        Year5 = Year4 * 4,

        %%io:format("PwDay: ~B, Day: ~B, Year: ~B~n", [PwDay, Day4, Year5]),

        if Day4 < 306 ->
                YDay = 1;
           true ->
                YDay = 0
        end,
        {Year6, Day5} = if Day4 =:= 1460 -> 
                {Year5 + 3, 365};
           true ->
                {Year5 + (Day4 div 365), Day4 rem 365}
        end,
        YDay0 = YDay + Day5,
        %%io:format("YDay: ~B, Day: ~B, Year: ~B~n", [YDay0, Day5, Year6]),

        Day6  = Day5 * 10,
        Month = (Day6 + 5) div 306,
        Day7  = (Day6 + 5) rem 306,
        Day8  = Day7 div 10,
        {YDay1, Year7, Month0} = if Month >= 10 ->
                {YDay0 - 306, Year6 + 1, Month - 10};
           true ->
                {YDay0 + 59, Year6, Month + 2}
        end,
        %%io:format("YDay: ~B, Day: ~B, Year: ~B~n", [YDay1, Day8, Year7]),

        CD = #caldate{ year  = Year7,
                       month = Month0 + 1,
                       day   = Day8 + 1
        },

        {CD, PwDay, YDay1}.


-spec dyupdate(integer(), integer()) -> {integer(), integer()}.

dyupdate(Day, Year) when Day >= 146097 ->
        dyupdate(Day - 146097, Year + 1);
dyupdate(Day, Year) ->
        {Day, Year}.


-spec scan(string()) -> #caldate{}.

scan(Str) ->
        O    = string:chr(Str, $-),
        Sign = if O ==  1 -> -1;
                     true -> 1
               end,
        [Year, Month, Day] = string:tokens(Str, "-"),

        {Y, []} = string:to_integer(Year),
        {M, []} = string:to_integer(Month),
        {D, []} = string:to_integer(Day),

        #caldate{
                year  = Sign * Y,
                month = M,
                day   = D
        }.


-spec fmt(#caldate{}) -> string().

fmt(CD) ->
        lists:flatten(io_lib:format("~B-~2..0B-~2..0B", 
                [CD#caldate.year, CD#caldate.month, CD#caldate.day])).


-spec normalize(#caldate{}) -> #caldate{}.

normalize(CD) -> 
        {CD0, _, _} = taider_caldate:frommjd(taider_caldate:mjd(CD)),
        CD0.


-spec easter(#caldate{}) -> #caldate{}.

easter(CD) ->
        Y = CD#caldate.year,

        C = (Y div 100) + 1,
        T = 210 - ((( C * 3) div 4) rem 210),
        J = Y rem 19,
        N = 57 - ((14 + J * 11 + (C * 8 + 5) div  25 + T) rem 30),

        if (N =:= 56) andalso (J > 10) ->
                N0 = N - 1;
           (N =:= 57) ->
                N0 = N - 1;      
           true ->
                N0 = N
        end,
        N1 = N0 - ((((Y rem 28) * 5) div 4 + T + N0 + 2 ) rem 7),

        if (N1 < 32) ->
                CD1 = #caldate{
                        year  = Y,
                        month = 3,
                        day   = N1
                };
           true ->
                CD1 = #caldate{
                        year  = Y,
                        month = 4,
                        day   = N1 - 31
                }
        end,

        CD1.
