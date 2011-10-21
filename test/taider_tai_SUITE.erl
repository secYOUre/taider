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

    ?line ?TAI_PACK = length(Bin),
    ?line Bin = lists:filter(fun(X) -> X =< 255 end, Bin),
    ok.

test_unpack() ->
    [{userdata,
        {doc, "Testing taider_tai:unpack()."}}].
test_unpack(_Config) ->
    ?line Now  = taider_tai:now(),
    ?line Bin  = taider_tai:pack(Now),
    ?line Now  = taider_tai:unpack(Bin),
    ok.
