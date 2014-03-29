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
-module(taider_caltime_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include("taider.hrl").

-include_lib("test_server/include/test_server.hrl").
%% -include_lib("common_test/include/ct.hrl").

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
          test_scan,
          test_tai,
          test_utc,
          test_fmt
         ].

dates() -> [
        "-1000000-12-31 23:59:59 +0000",
        "-999999-01-01 00:00:00 +0000",
        "-1-12-31 23:59:59 +0000",
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


test_scan() ->
    [{userdata,
        {doc, "Testing taider_caldate:scan()."}}].
test_scan(_Config) ->
    ?line Dates = dates(),
    ?line Dates = lists:map(
                fun(X) ->
                        {caltime,{caldate,Y,Month,D},H,Min,S,O} = 
                                                taider_caltime:scan(X),
                        Sign = if O < 0 -> "";
                                  true  -> "+"
                               end,
                        lists:flatten(
                              io_lib:format(
                              "~B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B ~s~4..0B", 
                              [Y,Month,D,H,Min,S,Sign,O]))
                end, Dates),
    ok.

test_tai() ->
    [{userdata,
        {doc, "Testing taider_caldate:tai()."}}].
test_tai(_Config) ->
    %%?line Id    = ?config(tid, Config),
    ?line {ok, Id} = taider_leapsecs:init(),
    ?line Dates = dates(),
    ?line Dates = lists:takewhile(
                fun(X) ->
                        C = taider_caltime:scan(X),
                        {tai, T} = taider_caltime:tai(C, Id),
                        T =:= T
                end, Dates),
    ok.

test_utc() ->
    [{userdata,
        {doc, "Testing taider_caldate:utc()."}}].
test_utc(_Config) ->
    ?line {ok, Id} = taider_leapsecs:init(),
    ?line Dates = dates(),
    ?line Dates = lists:takewhile(
                fun(X) ->
                        C = taider_caltime:scan(X),
                        T = taider_caltime:tai(C, Id),
                        {C0,_,_}  = taider_caltime:utc(T, Id),
                        C =:= C0
                end, Dates),
    ok.

test_fmt() ->
    [{userdata,
        {doc, "Testing taider_caldate:fmt()."}}].
test_fmt(_Config) ->
    ?line Dates = dates(),
    ?line Dates = lists:map(
                fun(X) ->
                        CT = taider_caltime:scan(X),
                        taider_caltime:fmt(CT)
                end, Dates),
    ok.
