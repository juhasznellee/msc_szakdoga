-module(sl_t1).
-export([get_max_num_spawn1/0]).

% FROM
get_max_num_spawn1() ->
    Pid = spawn(fun() ->
                        lists:max([2, 3])
                end),
    link(Pid),
    Pid ! ok.

% TO
% get_max_num_spawn1() ->
%     Pid = spawn_link(fun() ->
%                 lists:max([2, 3])
%             end),
%     Pid ! ok.