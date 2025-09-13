-module(exh_t5).
-export([f/0]).

% FROM
f() ->
    Pid = spawn(lists, max, [[2, 3]]),
    link(Pid),
    Pid! ok
.

% TO
% f() ->
%     Pid = spawn_link(lists, max, [[2, 3]]),
%     Pid! ok
% .