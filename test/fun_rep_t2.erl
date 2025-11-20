-module(fun_rep_t2).
-export([bta/0]).

bta() ->
    binary_to_existing_atom(<<"Erlang">>, utf8)
.
