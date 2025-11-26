-module(fun_rep_t4).
-export([t/0]).

% FROM
t() ->
    Bin = term_to_binary(hello),
    binary_to_term(Bin, [safe])
.

% TO - in the cmd
% The binary_to_term function is already safe.
% deny
