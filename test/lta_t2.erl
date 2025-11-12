-module(lta_t2).
-export([t/0]).

% FROM
t() ->
    [
        list_to_atom(integer_to_list(X)) || X<-lists:seq(1, 25)
    ].


% TO
% t() ->
%     case size_check(lists:seq(1, 25)
%             ) of
%         true ->
%             [
%             list_to_atom(integer_to_list(X)) || X<-lists:seq(1, 25)
%             ];
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 50.