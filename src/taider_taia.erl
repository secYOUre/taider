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


