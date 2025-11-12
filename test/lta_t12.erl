-module(lta_t12).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(filename:rootname(filename:basename(XS, "asdASD"))).

% TO
%"Function call not found."