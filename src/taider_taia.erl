
-module(taider_taia).

-include("taider.hrl").

-export([
        tai/1,
        now/0,
        add/2,
        sub/2,
        frac/1,
        approx/1,
        half/1,
        less/2,
        pack/1,
        unpack/1,
        fmtfrac/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("taider_constants.hrl").



-spec tai(#taia{}) -> #tai{}.

tai(TA) ->
        #tai{ x = TA#taia.sec}.


-spec now() -> #taia{}.

now() ->
        Now = {_Mega, _Sec0, Micro} = erlang:now(),
        Sec = ?UNIX_EPOCH_TAISEC +
              calendar:datetime_to_gregorian_seconds(
                calendar:now_to_universal_time(Now)) -
              ?UNIX_EPOCH_GREGORIAN,

        #taia{  sec = Sec,
               nano = 1000 * Micro + 500,
               atto = 0 }.


-spec add(#taia{}, #taia{}) -> #taia{}.

add(U, V) ->
        Sec  = U#taia.sec  + V#taia.sec,
        Nano = U#taia.nano + V#taia.nano,
        Atto = U#taia.atto + V#taia.atto,
        if Atto > 999999999 ->
                Atto1 = Atto - 1000000000,
                Nano1 = Nano + 1;
           true ->
                Atto1 = Atto,
                Nano1 = Nano
        end,
        if Nano1 > 999999999 ->
                Nano2 = Nano1 - 1000000000,
                Sec1  = Sec + 1;
           true ->
                Nano2 = Nano1,
                Sec1  = Sec
        end,

        #taia{
               sec  = Sec1,
               nano = Nano2,
               atto = Atto1
        }.
  
                   
-spec sub(#taia{}, #taia{}) -> #taia{}.

sub(U, V) ->
        Sec  = U#taia.sec  - V#taia.sec,
        Nano = U#taia.nano - V#taia.nano,
        Atto = U#taia.atto - V#taia.atto,
        if Atto > U#taia.atto ->
                Atto1 = Atto + 1000000000,
                Nano1 = Nano - 1;
           true ->
                Atto1 = Atto,
                Nano1 = Nano
        end,
        if Nano1 > U#taia.nano ->
                Nano2 = Nano1 + 1000000000,
                Sec1  = Sec - 1;
           true ->
                Nano2 = Nano1,
                Sec1  = Sec
        end,

        #taia{
               sec  = Sec1,
               nano = Nano2,
               atto = Atto1
        }.


-spec frac(#taia{}) -> number().

frac(TA) ->
        (TA#taia.atto * 0.000000001 + TA#taia.nano) * 0.000000001.


-spec approx(#taia{}) -> non_neg_integer().

approx(TA) ->
        taider_tai:approx(#tai{ x = TA#taia.sec }) + taider_taia:frac(TA).


-spec half(#taia{}) -> #taia{}.

half(U) -> 
        Atto = U#taia.atto bsr 1,
        if U#taia.nano band 1 ->
                Atto1 = Atto + 500000000;
           true ->
                Atto1 = Atto
        end,
        Nano = U#taia.nano bsr 1,
        if U#taia.sec band 1 ->
                Nano1 = Nano + 500000000;
           true ->
                Nano1 = Nano
        end,
        Sec = U#taia.sec bsr 1,

        #taia{
                sec = Sec,
                nano = Nano1,
                atto = Atto1
        }.


-spec less(#taia{}, #taia{}) -> boolean().

less(TA, U) ->
        if TA#taia.sec < U#taia.sec ->
                true;
           TA#taia.sec > U#taia.sec ->
                false;
           TA#taia.sec =:= U#taia.sec ->

                if TA#taia.nano < U#taia.nano ->
                        true;
                   TA#taia.nano > U#taia.nano ->
                        false;
                   TA#taia.nano =:= U#taia.nano ->

                        TA#taia.atto < U#taia.atto
                end
        end.


-spec pack(#taia{}) -> string().

pack(TA) ->
        SecP  = taider_tai:pack(#tai{ x = TA#taia.sec}),
        NanoP = taider_tai:pack(TA#taia.nano, [], 3),
        AttoP = taider_tai:pack(TA#taia.atto, [], 3),
        SecP ++ NanoP ++ AttoP.


-spec unpack(string()) -> #taia{}.

unpack(PTA) ->
        PSec  = lists:sublist(PTA, 1, 8),
        PNano = lists:sublist(PTA, 9, 4),
        PAtto = lists:sublist(PTA, 13,4),

        USec  = taider_tai:unpack(PSec),
        UNano = taider_tai:unpack(PNano),
        UAtto = taider_tai:unpack(PAtto),

        #taia{
                sec  = USec#tai.x,
                nano = UNano#tai.x,
                atto = UAtto#tai.x
        }.


-spec fmtfrac(#taia{}) -> string().

fmtfrac(TA) ->
        fracpack(TA#taia.nano, [], 8) ++ fracpack(TA#taia.atto, [], 8).


-spec fracpack(non_neg_integer(), string(), non_neg_integer()) -> string().

fracpack(X, P, 0) ->
        lists:reverse(lists:append(P, [$0 + (X rem 10)]));

fracpack(X, P, N) ->
        P1 = lists:append(P, [$0 + (X rem 10)]),
        fracpack(X div 10, P1, N -1).


