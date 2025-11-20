-module(t1).
-export([t/1]).

% FROM
t(X) ->
   list_to_atom(integer_to_list(X) ++ "my_pool")
.

megtalalni() ->
   [t(X) || X <- lists:seq(1, 25)]
.

% TO
% t(X) ->
%    list_to_atom(integer_to_list(X) ++ "my_pool")
% .

% megtalalni() ->
%     case size_check(lists:seq(1, 25)) of
%         true -> [t(X) || X<-lists:seq(1, 25)]
%         ;
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 50.