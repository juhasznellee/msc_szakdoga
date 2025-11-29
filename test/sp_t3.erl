-module(sp_t3).
-export([get_max_num_spawn3/0]).

% FROM
get_max_num_spawn3() ->
    Pid = spawn(lists, max, [[2, 3]]),
    link(Pid),
    Pid! ok.

% TO
% get_max_num_spawn3() ->
%     Pid = spawn_link(lists, max, [[2, 3]]),
%     Pid! ok.