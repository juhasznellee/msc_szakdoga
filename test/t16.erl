-module(t16).
-export([t/1]).

% FROM
t(XX) ->
    list_to_atom(lists:duplicate(XX, $a)).


megtalalni() ->
    [t(X) || X<-lists:seq(1, 25)].

% TO
% t(XX) ->
%     list_to_atom(lists:duplicate(XX, $a)).


% megtalalni() ->
%     case size_check(lists:seq(1, 25)) of
%         true -> [t(X) || X<-lists:seq(1, 25)]
%         ;
%         false -> throw("Variable criteria not met")
%     end.
% size_check(X) -> length(X) < 10000.