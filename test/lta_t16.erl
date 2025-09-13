-module(lta_t16).
-export([t/1]).

% FROM
t(Cmd) ->
    os:cmd(Cmd). 

% TO
% t(Cmd) ->
%     case sanitize(Cmd) of
%         true -> os:cmd(Cmd);
%         false -> throw("Variable criteria not met")
%     end.
% sanitize(X) -> length(X) < 50.