-module(lta_t20).
-export([t/1]).

% FROM

t(X) ->
   list_to_atom(integer_to_list(X))
.

% TO
%lta_t21-ben