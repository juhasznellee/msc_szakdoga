-module(lta_t5).
-export([t/0]).

% FROM
t() ->
lists:map(
        fun({KeyStr, Value}) -> {list_to_atom(KeyStr), Value} end, [{"id", 1}, {"name", "John"}]).

% TO
% t() ->
%     case size_check([{"id", 1}, {"name", "John"}]) of
%         true ->
%             lists:map(
%                 fun({KeyStr, Value}) -> {list_to_atom(KeyStr), Value} end,
%                 [{"id", 1}, {"name", "John"}]);
%         false -> throw("Variable criteria not met")
%     end.
% size_check(X) -> length(X) < 5000000.