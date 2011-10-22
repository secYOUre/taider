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
-module(taider_utils_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include("taider.hrl").

-include_lib("test_server/include/test_server.hrl").
%% -include_lib("common_test/include/ct.hrl").

%% -import(ct).
-import(lists).
% Let's error out if our tests take over a minute to complete. This can be reconfigured
% on a per testcase basis in init_per_testcase.
suite() -> [{timetrap, {minutes, 1}}].


init_per_suite(Config) ->
%%    {ok, Id} = taider_leapsecs:init(),
%%    [{tid, Id} | Config].
    Config.
    
end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
    
end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [
          test_nowutc,
          test_easter
         ].

dates() -> [
        "0-01-01 00:00:00 +0000",
        "1-01-01 01:01:01 +0000",
        "1000-01-01 12:04:37 +0000",
        "1799-12-31 23:59:59 +0000",
        "1899-12-31 23:59:59 +0000",
        "1900-01-01 00:00:00 +0000",
        "1969-12-31 23:59:49 +0000",
        "1969-12-31 23:59:50 +0000",
        "1969-12-31 23:59:59 +0000",
        "1970-01-01 00:00:00 +0000",
        "1970-01-01 00:00:01 +0000",
        "1972-06-30 23:59:58 +0000",
        "1972-06-30 23:59:59 +0000",
        "1972-06-30 23:59:60 +0000",
        "1972-07-01 00:00:00 +0000",
        "1972-07-01 00:00:01 +0000",
        "1995-12-31 23:59:58 +0000",
        "1995-12-31 23:59:59 +0000",
        "1995-12-31 23:59:60 +0000",
        "1996-01-01 00:00:00 +0000",
        "1996-01-01 00:00:01 +0000",
        "1996-01-01 00:00:02 +0000",
        "1997-06-30 23:59:59 +0000",
        "1997-06-30 23:59:60 +0000",
        "1997-07-01 00:00:00 +0000",
        "1997-07-30 08:57:43 +0000",
        "1997-10-03 18:14:48 +0000",
        "1999-09-09 09:09:09 +0000",
        "1999-12-31 23:59:59 +0000",
        "2000-01-01 00:00:00 +0000",
        "2000-01-01 00:00:01 +0000",
        "2000-02-28 23:59:59 +0000",
        "2000-02-29 23:59:59 +0000",
        "2000-03-01 00:00:00 +0000",
        "2000-12-31 23:59:59 +0000",
        "2100-01-01 17:42:15 +0000",
        "2200-01-01 17:42:15 +0000",
        "3000-01-01 17:42:15 +0000",
        "10000-01-01 17:42:15 +0000",
        "999999-12-31 23:59:59 +0000",
        "1000000-01-01 00:00:00 +0000"
        ].


easters()-> [
         "Sun 0-04-09 yearday 99  mjd -678842",
	 "Sun 1-04-01 yearday 90  mjd -678485",
	 "Sun 1000-03-30 yearday 89  mjd -313610",
	 "Sun 1799-03-24 yearday 82  mjd -21787",
	 "Sun 1899-04-02 yearday 91  mjd 14746",
	 "Sun 1900-04-15 yearday 105  mjd 15124",
	 "Sun 1969-04-06 yearday 95  mjd 40317",
	 "Sun 1969-04-06 yearday 95  mjd 40317",
	 "Sun 1969-04-06 yearday 95  mjd 40317",
	 "Sun 1970-03-29 yearday 87  mjd 40674",
	 "Sun 1970-03-29 yearday 87  mjd 40674",
	 "Sun 1972-04-02 yearday 92  mjd 41409",
	 "Sun 1972-04-02 yearday 92  mjd 41409",
	 "Sun 1972-04-02 yearday 92  mjd 41409",
	 "Sun 1972-04-02 yearday 92  mjd 41409",
	 "Sun 1972-04-02 yearday 92  mjd 41409",
	 "Sun 1995-04-16 yearday 105  mjd 49823",
	 "Sun 1995-04-16 yearday 105  mjd 49823",
	 "Sun 1995-04-16 yearday 105  mjd 49823",
	 "Sun 1996-04-07 yearday 97  mjd 50180",
	 "Sun 1996-04-07 yearday 97  mjd 50180",
	 "Sun 1996-04-07 yearday 97  mjd 50180",
	 "Sun 1997-03-30 yearday 88  mjd 50537",
	 "Sun 1997-03-30 yearday 88  mjd 50537",
	 "Sun 1997-03-30 yearday 88  mjd 50537",
	 "Sun 1997-03-30 yearday 88  mjd 50537",
	 "Sun 1997-03-30 yearday 88  mjd 50537",
	 "Sun 1999-04-04 yearday 93  mjd 51272",
	 "Sun 1999-04-04 yearday 93  mjd 51272",
	 "Sun 2000-04-23 yearday 113  mjd 51657",
	 "Sun 2000-04-23 yearday 113  mjd 51657",
	 "Sun 2000-04-23 yearday 113  mjd 51657",
	 "Sun 2000-04-23 yearday 113  mjd 51657",
	 "Sun 2000-04-23 yearday 113  mjd 51657",
	 "Sun 2000-04-23 yearday 113  mjd 51657",
	 "Sun 2100-03-28 yearday 87  mjd 88155",
	 "Sun 2200-04-06 yearday 96  mjd 124688",
	 "Sun 3000-04-13 yearday 103  mjd 416889",
	 "Sun 10000-04-16 yearday 106  mjd 2973590",
	 "Sun 999999-03-28 yearday 86  mjd 364563280",
	 "Sun 1000000-04-16 yearday 106  mjd 364563665"
         ].

test_nowutc() ->
    [{userdata,
        {doc, "Testing taider_utils:nowutc()."}}].
test_nowutc(_Config) ->
    ?line NUTC  = taider_utils:nowutc(),
    ?line [Date, Time] = string:tokens(NUTC, "\s\t"),

    ?line CD   = taider_caldate:scan(Date),
    ?line Date = taider_caldate:fmt(CD),

    ?line [H,M,S] = string:tokens(Time, ":"),
    ?line {_Hour, []} = string:to_integer(H),
    ?line {_Min, []}  = string:to_integer(M),
    ?line {_Sec, []}  = string:to_float(S),
    ok.

test_easter() ->
    [{userdata,
        {doc, "Testing taider_utils:easter()."}}].
test_easter(_Config) ->
    ?line Dates   = dates(),
    ?line Easters = easters(),
    ?line Easters = lists:map(
                fun(X) ->
                        ?line {caltime,{caldate,Year,_,_},_,_,_,_} 
                                = taider_caltime:scan(X),
                        taider_utils:easter(Year)
                end, Dates),
    ok.
                        
