-module(lta_t12).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(filename:rootname(filename:basename(XS, "asdASD"))).

% TO
% t(XS) ->
%     case sanitize(XS) of
%         true ->
%             list_to_atom(filename:rootname(
%                     filename:basename(XS, "asdASD")));
%         false -> throw("Variable criteria not met")
%     end.
% sanitize(X) -> length(X) < 50.
