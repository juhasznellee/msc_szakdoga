-module(lta_t7).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(lists:append("V", XS)).


% TO
%"Function call not found."
