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
-module(taider_leapsecs).

-include("../include/taider.hrl").

-export([
        load/0,
        init/0,
        read/0,
        add/3,
        sub/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% -include("file.hrl").


-record(file_descriptor,
        {module :: module(),     % Module that handles this kind of file
         data   :: term()}).     % Module dependent data

%%  Types
-type io_device() :: pid() | #file_descriptor{}.


-spec load() -> 'ok' | {'error', string()}.

load() ->
        Leapsecs = filename:join([filename:dirname(
                        code:which(?MODULE)), "..", "data", "leapsecs.txt"]),
        Dat      = filename:join([filename:dirname(
                        code:which(?MODULE)), "..", "data", "leapsecs.dat"]),

        {ok, S0} = file:open(Dat, write),

        case file:open(Leapsecs, read) of
                {ok, S}    ->
                        load_leapsecs(S, S0, 0);

                {error, Error} ->
                        {error, Error}
        end.

-spec load_leapsecs(io_device(), io_device(), non_neg_integer()) -> 'ok'.

load_leapsecs(S, S0, Leaps) ->
        case io:get_line(S, '') of
                eof  ->
                        file:close(S),
                        file:close(S0);
                Line ->
                        case string:sub_string(Line, 1, 1) of
                                "+" ->
                                        Line1  = string:strip(Line, right, $\n),
                                        CD = taider_caldate:scan(Line1),
                                        T = #tai{ x=(taider_caldate:mjd(CD)+1)
                                                   * 86400 
                                                   + 4611686014920671114
                                                   + Leaps
                                        },
                                        Leaps1 = Leaps + 1,
                                        %%io:format("~B~n", [T#tai.x]),
                                        TP = taider_tai:pack(T),
                                        io:format(S0, "~s", [TP]),

                                        load_leapsecs(S, S0, Leaps1);
                                _   ->
                                        load_leapsecs(S, S0, Leaps)
                        end
        end,
        ok.


-spec init() -> 'ok' | {'error', string()}.

init() -> read().


-spec read() -> 'ok' | {'error', string()}.

read() ->        
        Dat      = filename:join([filename:dirname(
                        code:which(?MODULE)), "..", "data", "leapsecs.dat"]),

        TableId  = ets:new(taider_ets, [bag, private, {keypos, 1}]),
        case file:open(Dat, read) of
                {ok, S}        ->
                        {ok, Id} = read_leapsecs(S, TableId, 0),
                        {ok, Id};

                {error, enoent} ->
                        %% if the .dat file is not there, try to build
                        %% a new one
                        load(), 
                        O = case file:open(Dat, read) of
                                {ok, S1} ->
                                        {ok, Id2} = read_leapsecs(S1,TableId,0),
                                        {ok, Id2};
                                {error, Error2} ->
                                        {error, Error2}
                        end,
                        O;

                {error, Error} ->
                        {error, Error}
        end.


-spec read_leapsecs(io_device(), non_neg_integer(), non_neg_integer()) -> {'ok', non_neg_integer()}.

read_leapsecs(S, TableId, Offset) ->
        case file:pread(S, Offset, 8) of 
                eof     ->
                        file:close(S);
                {error, _Error} ->
                        %%io:format("Error: ~p~n", [Error]),
                        file:close(S);
                {ok, R} ->
                        %%io:format("Unpacking: ~p~n", [R]),
                        TAI = taider_tai:unpack(R),
                        %% ets:insert(TableId, {tai, TAI#tai.x}),
                        ets:insert(TableId, TAI),
                        read_leapsecs(S, TableId, Offset + 8 )
        end,
        {ok, TableId}.


-spec add(#tai{}, boolean(), non_neg_integer()) -> #tai{}.

add(TAI, Hit, TableId) ->
        U = TAI#tai.x,
        
        Leapsecs     = ets:lookup(TableId, tai),        
        U1 = add_leapseconds(Hit, Leapsecs, U),

        #tai{ x = U1}.                   


%%-spec add_leapseconds(boolean(), [#tai{}]) -> integer().
%%
%%add_leapseconds(Hit, Leapseconds) ->
%%        add_leapseconds(Hit, Leapseconds, 0).


-spec add_leapseconds(boolean(), [#tai{}], integer()) -> integer().

add_leapseconds(_Hit, [], Sum) ->
        Sum;

add_leapseconds(Hit, [H|T], Sum) ->
        if Sum < H#tai.x -> 
                        Sum;
                    true -> 
                        if (not Hit) or (Sum > H#tai.x) ->
                                        Sum1 = Sum + 1;
                                                true    ->
                                        Sum1 = Sum
                        end,
                        add_leapseconds(Hit, T, Sum1)
        end.                



-spec sub(#tai{}, non_neg_integer()) -> #tai{}.

sub(TAI, TableId) ->
        U = TAI#tai.x,

        Leapsecs     = ets:lookup(TableId, tai),
        GreaterLeaps = lists:filter(fun(A) -> U >= A#tai.x end, Leapsecs),

        {Results, _} = lists:mapfoldl(fun(G, S) ->
                               S1 = S + 1,
                               if U =:= G#tai.x ->
                                        {{ #tai{ x = U - S1}, true }, S1};
                                  true          ->
                                        {{ #tai{ x = U - S1}, false}, S1}
                               end end,
                               0, GreaterLeaps), 

        Results0     = lists:append([{TAI, false}], Results),

        {Leaps, NLeaps} = lists:partition(
                                fun({_TAI, Flag}) ->                        
                                        Flag =:= true

                                end, Results0),
        {{tai, X}, Flag} = if length(Leaps) > 0 ->
                        lists:nth(1, Leaps);
               true              ->
                        lists:last(NLeaps)
            end,

        {#tai{ x = X}, Flag}.
                               
