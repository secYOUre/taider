
-module(taider_tai).

-include("../include/taider.hrl").

-export([
        now/0,
        add/2,
        sub/2,
        approx/1,
        less/2,
        pack/1,
        pack/3,
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
        pack(T#tai.x, [], 7).

-spec pack(non_neg_integer(), string(), non_neg_integer()) -> string().

pack(X, P, 0) ->
        %%io:format("pack: X: ~B, P: ~p, N: ~B~n", [X, P, 0]),
        lists:append(P, [X]);

pack(X, P, N) ->
        %%io:format("pack: X: ~B, P: ~p, N: ~B~n", [X, P, N]),
        P1 = lists:append(P, [X band 255]),
        pack(X bsr 8, P1, N -1).


-spec unpack(string()) -> #tai{}.

unpack(P) ->
        Reversed = lists:reverse(P),
        U  = lists:foldl(fun(S, X) -> X1 = X bsl 8, X1 + S end, 
                    lists:nth(1, Reversed), 
                    lists:nthtail(1, Reversed)),

        #tai{ x = U}.
