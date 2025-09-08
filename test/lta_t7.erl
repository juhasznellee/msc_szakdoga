-module(lta_t7).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(lists:append("V", XS)).

% TO
% t(XS) ->
%     case sanitize(XS) of
%         true -> list_to_atom(lists:append("V", XS));
%         false -> throw("Variable criteria not met")
%     end.
% sanitize(X) -> length(X) < 50.