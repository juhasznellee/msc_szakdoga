-module(lta_t3).
-export([t/1]).

% FROM
t(XS) ->
    [list_to_atom(X) || X <- XS].


% TO
% t(XS) ->
%     case sanitize(XS) of
%         true -> [list_to_atom(X) || X<-XS];
%         false -> throw("Variable criteria not met")
%     end.
% sanitize(X) -> length(X) < 50.