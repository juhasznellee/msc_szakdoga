-module(spl_test).
-export([run/0, task/0]).

% FROM
run() ->
    Pid = spawn(spl_test, task, []),
    link(Pid),
    Pid ! {start, <<"test data">>},
    timer:sleep(1000),
    Pid ! stop,
    ok.

task() ->
    receive
        {start, Data} ->
            io:format("Got data: ~p~n", [Data]),
            task();
        stop ->
            io:format("Stopping.~n"),
            ok
    end.

% TO
% run() ->
%     Pid = spawn_link(spl_test, task, []),
%     Pid ! {start, <<"test data">>},
%     timer:sleep(1000),
%     Pid ! stop,
%     ok.

% task() ->
%     receive
%         {start, Data} ->
%             io:format("Got data: ~p~n", [Data]),
%             task();
%         stop ->
%             io:format("Stopping.~n"),
%             ok
%     end.
