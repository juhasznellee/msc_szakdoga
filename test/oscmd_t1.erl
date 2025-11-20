-module(oscmd_t1).
-export([t/1]).

% FROM
t(Cmd) ->
   os:cmd("less " ++ Cmd).

% TO
% t(Cmd) ->
%     case check_input("less " ++ Cmd) of
%         true -> throw("Variable criteria not met");
%         false -> os:cmd("less " ++ Cmd)
%     end.

% check_input(X) ->
%     lists:any(fun(E) -> string:find(X, E) =/= nomatch end,
%         [";", "&&", "|"]).
