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
-module(taider_tai).

-include("../include/taider.hrl").

-export([
        now/0,
        add/2,
        sub/2,
        approx/1,
        less/2,
        pack/1,
        unpack/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("./taider_constants.hrl").


-spec now() -> #tai{}.
 
now() ->
        Now = erlang:now(),
        Sec = ?UNIX_EPOCH_TAISEC 
            + calendar:datetime_to_gregorian_seconds(
                calendar:now_to_universal_time(Now)) 
            - ?UNIX_EPOCH_GREGORIAN,

        #tai{ x = Sec}.


-spec add(#tai{}, #tai{}) -> #tai{}.

add(U, V) ->
        #tai{ x = U#tai.x + V#tai.x}.


-spec sub(#tai{}, #tai{}) -> #tai{}.

sub(U, V) ->
        #tai{ x = U#tai.x - V#tai.x}.        


-spec approx(#tai{}) -> non_neg_integer().

approx(T) ->
        T#tai.x.


-spec less(#tai{}, #tai{}) -> boolean().

less(T, U) ->
        T#tai.x < U#tai.x.


-spec pack(#tai{}) -> string().

pack(T) ->
        X = T#tai.x,
        <<X:64>>.

-spec unpack(string()|binary()) -> #tai{}.

unpack(P) when is_list(P) ->
        unpack(list_to_binary(P));
unpack(P) when is_binary(P) ->
        <<X:64>> = P,
        #tai{ x = X}.
