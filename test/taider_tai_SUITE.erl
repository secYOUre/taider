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
-module(taider_tai_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include("taider.hrl").

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
          test_now,
          test_add,
          test_sub,
          test_approx,
          test_less,
          test_pack,
          test_unpack
         ].

test_now() ->
    [{userdata,
        {doc, "Testing taider_tai:now()."}}].
test_now(_Config) ->
    ?line {tai, _} = taider_tai:now(),
    ok.

test_add() ->
    [{userdata,
        {doc, "Testing taider_tai:add()."}}].
test_add(_Config) ->
    ?line {tai, Now}  = taider_tai:now(),
    ?line {tai, Then} = taider_tai:now(),
    ?line Result = Now + Then,

    ?line {tai, Result} = taider_tai:add({tai, Now}, {tai, Then}),
    ok.

test_sub() ->
    [{userdata,
        {doc, "Testing taider_tai:sub()."}}].
test_sub(_Config) ->
    ?line {tai, Now}  = taider_tai:now(),
    ?line {tai, Then} = taider_tai:now(),
    ?line Result = Then - Now,

    ?line {tai, Result} = taider_tai:sub({tai, Then}, {tai, Now}),
    ok.

test_approx() ->
    [{userdata,
        {doc, "Testing taider_tai:approx()."}}].
test_approx(_Config) ->
    ?line {tai, Now}  = taider_tai:now(),
    ?line Now         = taider_tai:approx({tai, Now}),
    ok.

test_less() ->
    [{userdata,
        {doc, "Testing taider_tai:less()."}}].
test_less(_Config) ->
    ?line Now  = taider_tai:now(),
    ?line timer:sleep(1000),
    ?line Then = taider_tai:now(),

    ?line true = taider_tai:less(Now, Then),
    ok.
    
test_pack() ->
    [{userdata,
        {doc, "Testing taider_tai:pack()."}}].
test_pack(_Config) ->
    ?line Now  = taider_tai:now(),
    ?line Bin  = taider_tai:pack(Now),

    ?line ?TAI_PACK = size(Bin),
    ?line L = binary_to_list(Bin),
    ?line L = lists:filter(fun(X) -> X =< 255 end, L),
    ok.

test_unpack() ->
    [{userdata,
        {doc, "Testing taider_tai:unpack()."}}].
test_unpack(_Config) ->
    ?line Now  = taider_tai:now(),
    ?line Bin  = taider_tai:pack(Now),
    ?line Now  = taider_tai:unpack(Bin),
    ok.
