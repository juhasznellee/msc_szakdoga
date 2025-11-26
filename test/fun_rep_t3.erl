-module(fun_rep_t3).
-export([t/0]).

% FROM
t() ->
    Bin = term_to_binary(hello),
    binary_to_term(Bin).

% TO
% t() ->
%   Bin = term_to_binary(hello),
%   binary_to_term(Bin, [safe]).