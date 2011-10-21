-module(taider_taia_SUITE).
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
          test_tai,
          test_now,
          test_add,
          test_sub,
          test_frac,
          test_approx,
          test_half,
          test_less,
          test_pack,
          test_unpack,
          test_fmtfrac
         ].

test_tai() ->
    [{userdata,
        {doc, "Testing taider_taia:tai()."}}].
test_tai(_Config) ->
    ?line Now      = taider_taia:now(),
    ?line {tai, _} = taider_taia:tai(Now),
    ok.

test_now() ->
    [{userdata,
        {doc, "Testing taider_taia:now()."}}].
test_now(_Config) ->
    ?line {taia, _, _, _} = taider_taia:now(),
    ok.

test_add() ->
    [{userdata,
        {doc, "Testing taider_taia:add()."}}].
test_add(_Config) ->
    ?line {taia, SecA, NanoA, AttoA}  = taider_taia:now(),
    ?line {taia, SecB, NanoB, AttoB}  = taider_taia:now(),
    ?line Sec  = SecA + SecB,
    ?line Nano = NanoA + NanoB,
    ?line Atto = AttoA + AttoB, 
    {Atto1, Nano1} = if Atto >  999999999 ->
            {Atto -  1000000000, Nano + 1};
       true ->
            {Atto, Nano}
    end,
    {Nano2, Sec1} = if Nano1 >  999999999 ->
            {Nano1 -  1000000000, Sec + 1};
       true ->
            {Nano1, Sec}
    end,

    ?line {taia, Sec1, Nano2, Atto1} = taider_taia:add(
                                {taia, SecA, NanoA, AttoA}, 
                                {taia, SecB, NanoB, AttoB}),
    ok.

test_sub() ->
    [{userdata,
        {doc, "Testing taider_taia:sub()."}}].
test_sub(_Config) ->
    ?line {taia, SecA, NanoA, AttoA}  = taider_taia:now(),
    ?line {taia, SecB, NanoB, AttoB}  = taider_taia:now(),
    ?line Sec  = SecA - SecB,
    ?line Nano = NanoA - NanoB,
    ?line Atto = AttoA - AttoB, 
    {Atto1, Nano1} = if Atto >  AttoA ->
            {Atto +  1000000000, Nano - 1};
       true ->
            {Atto, Nano}
    end,
    {Nano2, Sec1} = if Nano1 >  NanoA ->
            {Nano1 +  1000000000, Sec - 1};
       true ->
            {Nano1, Sec}
    end,

    ?line {taia, Sec1, Nano2, Atto1} = taider_taia:sub(
                                {taia, SecA, NanoA, AttoA}, 
                                {taia, SecB, NanoB, AttoB}),
    ok.


test_frac() ->
    [{userdata,
        {doc, "Testing taider_taia:frac()."}}].
test_frac(_Config) ->
    ?line {taia, Sec, Nano, Atto}  = taider_taia:now(),
    ?line Result = (Atto * 0.000000001 + Nano) * 0.000000001,
    ?line Result = taider_taia:frac({taia, Sec, Nano, Atto}),
    ok.

test_approx() ->
    [{userdata,
        {doc, "Testing taider_taia:approx()."}}].
test_approx(_Config) ->
    ?line {taia, Sec, Nano, Atto}  = taider_taia:now(),
    ?line Result = Sec + (Atto * 0.000000001 + Nano) * 0.000000001,
    ?line Result = taider_taia:approx({taia, Sec, Nano, Atto}),
    ok.

test_half() ->
    [{userdata,
        {doc, "Testing taider_taia:half()."}}].
test_half(_Config) ->
    ?line {taia, Sec, Nano, Atto}  = taider_taia:now(),
    ?line HalfAtto  = Atto bsr 1,
    ?line HalfAtto1 = if Nano band 1 ->
                                HalfAtto + 500000000;
                        true ->
                                HalfAtto
                      end,
    ?line HalfNano = Nano bsr 1,
    ?line HalfNano1 = if Sec band 1 ->
                                HalfNano + 500000000;
                        true ->
                                HalfNano
                      end,
    ?line HalfSec = Sec bsr 1,

    ?line {taia, HalfSec, HalfNano1, HalfAtto1} =
          taider_taia:half({taia, Sec, Nano, Atto}),
    ok.

test_less() ->
    [{userdata,
        {doc, "Testing taider_taia:less()."}}].
test_less(_Config) ->
    ?line Now  = taider_taia:now(),
    ?line timer:sleep(1000),
    ?line Then = taider_taia:now(),

    ?line true = taider_taia:less(Now, Then),
    ok.
    
test_pack() ->
    [{userdata,
        {doc, "Testing taider_taia:pack()."}}].
test_pack(_Config) ->
    ?line Now  = taider_taia:now(),
    ?line Bin  = taider_taia:pack(Now),

    ?line ?TAIA_PACK = length(Bin),
    ?line Bin = lists:filter(fun(X) -> X =< 255 end, Bin),
    ok.

test_unpack() ->
    [{userdata,
        {doc, "Testing taider_taia:unpack()."}}].
test_unpack(_Config) ->
    ?line Now  = taider_taia:now(),
    ?line Bin  = taider_taia:pack(Now),
    ?line Now  = taider_taia:unpack(Bin),
    ok.

test_fmtfrac() ->
    [{userdata,
        {doc, "Testing taider_taia:fmtfrac()."}}].
test_fmtfrac(_Config) ->
    ?line Now  = taider_taia:now(),
    ?line F    = taider_taia:fmtfrac(Now),

    ?line ?TAIA_FMTFRAC = length(F),
    ?line true = is_number(list_to_integer(F)),
    ok.
