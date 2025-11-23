-module(fun_rep_t2).
-export([bta/0]).

bta() ->
    binary_to_existing_atom(<<"Erlang">>, utf8)
.

% TO
% The binary_to_existing_atom function is already safe.
% deny