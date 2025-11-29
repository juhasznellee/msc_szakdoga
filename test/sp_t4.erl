-module(sp_t4).
-export([get_max_num_spawn4/0]).

% FROM
get_max_num_spawn4() ->
    Pid = spawn('some_node@host', lists, max, [[2, 3]]),
    link(Pid),
    Pid ! ok.

% TO
% get_max_num_spawn4() ->
%     Pid = spawn_link('some_node@host', lists, max, [[2, 3]]),
%     Pid ! ok.
