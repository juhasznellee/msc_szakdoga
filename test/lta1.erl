-module(lta1).
-export([list_comp_gen/0]).

% FROM
list_comp_gen() ->
    [list_to_atom(integer_to_list(X)) || X <- lists:seq(1, 5000000)].

% TO
% list_comp_gen() ->
%     case size_check(lists:seq(1, 5000000)) of
%         true -> [list_to_atom(integer_to_list(X)) || X<-lists:seq(1, 5000000)];
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 5000000.