-module(lta_t11).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(filename:rootname(filename:basename((XS)))).

% TO
%"Function call not found."