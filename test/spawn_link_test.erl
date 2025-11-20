-module(spawn_link_test).
-export([get_max_num/0]).

% FROM
get_max_num() ->
    Pid = spawn(lists, max, [[2, 3]]),
    link(Pid),
    Pid! ok
.

% TO
% get_max_num() ->
    % Pid = spawn_link(lists, max, [[2, 3]]),
    % Pid! ok
% .