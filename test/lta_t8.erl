-module(lta_t8).
-export([t/1]).

% FROM
t(XS) ->
    list_to_atom(lists:concat(XS)).

% TO
%"Function call not found."