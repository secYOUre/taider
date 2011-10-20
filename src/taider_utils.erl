%% Copyright (c) 2006-2011, Alfonso De Gregorio, Security Pillar Ltd (secYOUre)
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without 
%% modification, are permitted provided that the following conditions 
%% are met:
%% 
%%     * Redistributions of source code must retain the above 
%%       copyright notice, this list of conditions and the 
%%       following disclaimer.
%%     * Redistributions in binary form must reproduce the above 
%%       copyright notice, this list of conditions and the following 
%%       disclaimer in the documentation and/or other materials provided 
%%       with the distribution.
%%     * Neither the name of Security Pillar Ltd nor the names of its 
%%       contributors may be used to endorse or promote products derived 
%%       from this software without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
%% COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
%% OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
%% USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% $Id$
%%
%% @copyright 2011 Alfonso De Gregorio
%% @author Alfonso De Gregorio <adg@crypto.lo.gy>
%% @version {@version}
%%
%% @doc
%%
%% @end
%% =====================================================================

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
