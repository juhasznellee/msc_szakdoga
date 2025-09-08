-module(exh_t1).
-export([t/0]).

% FROM
t() ->
    [list_to_atom(integer_to_list(X)) || X <- lists:seq(1, 25)]
.

% TO
% t() ->
%     [list_to_existing_atom(integer_to_list(X)) || X <- lists:seq(1, 25)]
% .