-module(example2).
-export([unsafe_ets/0]).

unsafe_ets() ->
    Table = ets:new(t, [set, public]),
    ets:insert(Table, {k, 1}),
    spawn(
        fun() ->
            ets:delete(Table, k)
        end
    ),
    Result = ets:lookup(Table, k),
    io:format("Lekérdezés eredménye: ~p~n", [Result])
.