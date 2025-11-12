-module(lta_t10).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(lists:append(XS, "ASD")).

% TO
%"Function call not found."