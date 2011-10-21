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
-module(taider_caldate_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include("taider.hrl").

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

-import(ct).
-import(lists).
% Let's error out if our tests take over a minute to complete. This can be reconfigured
% on a per testcase basis in init_per_testcase.
suite() -> [{timetrap, {minutes, 1}}].


init_per_suite(Config) ->
    Config.
    
end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
    
end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [
          test_scan,
          test_mjd,
          test_frommjd,
          test_fmt,
          test_normalize,
          test_easter
         ].

dates() -> [
        "2011-10-21",
        "1970-01-01",
        "-100-01-01",
        "2100-12-31",
        "10001-10-01"
        ].

mjd() -> [
        55855,
        40587,
        -715465,
        88433,
        2974123
        ].

cdwy() -> [
        {{caldate,2011,10,21},5,293},
        {{caldate,1970,1,1},4,0},
        {{caldate,-100,1,1},1,0},
        {{caldate,2100,12,31},5,365},
        {{caldate,10001,10,1},1,273}
        ].

easter() -> [
        {caldate,2011,4,24},
        {caldate,1970,3,29},
        {caldate,-100,4,1},
        {caldate,2100,3,28},
        {caldate,10001,4,8}
        ].


test_scan() ->
    [{userdata,
        {doc, "Testing taider_caldate:scan()."}}].
test_scan(_Config) ->
    ?line Dates = dates(),
    ?line Dates = lists:map(
                fun(X) ->
                        {caldate,Y,M,D} = taider_caldate:scan(X),
                        lists:flatten(
                              io_lib:format("~B-~2..0B-~2..0B", [Y,M,D]))
                end, Dates),
    ok.

test_mjd() ->
    [{userdata,
        {doc, "Testing taider_caldate:mjd()."}}].
test_mjd(_Config) ->
    ?line Dates = dates(),
    ?line MJD   = mjd(),
    ?line DMJD  = lists:zip(Dates, MJD),

    ?line DMJD  = lists:map(
                fun({Date,Day}) ->
                        CD = taider_caldate:scan(Date),
                        J = taider_caldate:mjd(CD),

                        {Date, J}
                end, DMJD),
    ok.

test_frommjd() ->
    [{userdata,
        {doc, "Testing taider_caldate:frommjd()."}}].
test_frommjd(_Config) ->
    ?line Dates = dates(),
    ?line MJD   = mjd(),
    ?line CDWY  = cdwy(),
    ?line DMJD  = lists:zip(Dates, MJD),
    
    ?line CDWY  = lists:map(
                fun({Date,Day}) ->
                        CD = taider_caldate:scan(Date),
                        J  = taider_caldate:mjd(CD),

                        taider_caldate:frommjd(J)
                end, DMJD),
    ok.

test_fmt() ->
    [{userdata,
        {doc, "Testing taider_caldate:fmt()."}}].
test_fmt(_Config) ->
    ?line Dates = dates(),
    ?line Dates = lists:map(
                fun(X) ->
                        CD = taider_caldate:scan(X),
                        taider_caldate:fmt(CD)
                end, Dates),
    ok.

test_normalize() ->
    [{userdata,
        {doc, "Testing taider_caldate:normalize()."}}].
test_normalize(_Config) ->
    ?line CDWY = cdwy(),
    ?line CDWY = lists:map(
                fun({CD,W,Y}) ->
                        {CD0, _, _} = taider_caldate:frommjd(
                                        taider_caldate:mjd(CD)),
                        {CD0,W,Y}
                end, CDWY),
    ok.

test_easter() ->
    [{userdata,
        {doc, "Testing taider_caldate:easter()."}}].
test_easter(_Config) ->
    ?line Dates = dates(),
    ?line Easter= easter(),
    ?line Easter= lists:map(
                fun(X) ->
                        CD  = taider_caldate:scan(X),
                        taider_caldate:easter(CD)
                end, Dates),
    ok.
