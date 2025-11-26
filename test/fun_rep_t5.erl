-module(fun_rep_t5).
-export([t/0]).

% FROM
t() ->
    Bin = term_to_binary(hello),
    erlang:binary_to_term(Bin).

% TO
% t() ->
%     Bin = term_to_binary(hello),
%     erlang:binary_to_term(Bin, [safe]).
