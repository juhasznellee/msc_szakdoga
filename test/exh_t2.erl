-module(exh_t2).
-export([t/0]).

% FROM
t() ->
    binary_to_atom(<<"Erlang">>, utf8)
.

% TO
% t() ->
%     binary_to_existing_atom(<<"Erlang">>, utf8)
% .