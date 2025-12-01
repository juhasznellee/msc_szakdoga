-module(lta4).
-export([high_order_atom_gen/0]).

% FROM
high_order_atom_gen() ->
   lists:map(fun(X) -> list_to_atom(integer_to_list(X)) end, lists:seq(1, 10)).


% TO
