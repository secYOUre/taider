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
-module(taider_leapsecs_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include("taider.hrl").

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

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
          test_load,
          test_init,
          test_read,
          test_add,
          test_sub
         ].

test_load() ->
    [{userdata,
        {doc, "Testing taider_leapsecs:load()."}}].
test_load(_Config) ->
    ?line Dat      = filename:join([filename:dirname(
                    code:which(?MODULE)), "..", "data", "leapsecs.dat"]),
    ?line file:delete(Dat),
    ?line ok = taider_leapsecs:load(),

    ?line {ok, FileInfo} = file:read_file_info(Dat),
    ?line 0 = FileInfo#file_info.size rem ?TAI_PACK,
    ok.

test_init() ->
    [{userdata,
        {doc, "Testing taider_leapsecs:init()."}}].
test_init(_Config) ->
    ?line {ok, Id} = taider_leapsecs:init(),
    ?line Leapsecs = ets:tab2list(Id),
    ?line 26 = length(Leapsecs),
    ?line Leapsecs = lists:filter(
                fun({tai, L}) -> L >= 4611686018506184714 end, 
                Leapsecs),
    ok.

test_read() ->
    [{userdata,
        {doc, "Testing taider_leapsecs:read()."}}].
test_read(Config) ->
    ?line ok = test_init(Config),
    ok.

test_add() ->
    [{userdata,
        {doc, "Testing taider_leapsecs:add()."}}].
test_add(_Config) ->
    ?line {ok, Id} = taider_leapsecs:init(),
    ?line Leapsecs = length(ets:lookup(Id, tai)),

    ?line {tai, Now} = taider_tai:now(),
    ?line {tai, LNo} = taider_leapsecs:add({tai, Now}, false, Id),

    ?line Leapsecs = LNo - Now,
    ok.

test_sub() ->
    [{userdata,
        {doc, "Testing taider_leapsecs:sub()."}}].
test_sub(_Config) ->
    ?line {ok, Id} = taider_leapsecs:init(),
    ?line Leapsecs = length(ets:lookup(Id, tai)),

    ?line {tai, Now} = taider_tai:now(),
    ?line {{tai, LNo}, Bool} = taider_leapsecs:sub({tai, Now}, Id),
    ?line true = is_boolean(Bool),

    ?line LNo = Now - Leapsecs,
    ok.
