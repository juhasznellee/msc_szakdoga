-module(sl_t2).
-export([get_max_num_spawn2/0]).

% FROM
get_max_num_spawn2() ->
    Pid = spawn(?MODULE, helper, []),
    link(Pid),
    Pid ! ok.

helper() ->
    lists:max([2,3]).

% TO
% get_max_num_spawn2() ->
%     Pid = spawn_link(?MODULE , helper, []),
%     Pid ! ok.

% helper() ->
%     lists:max([2,3]).