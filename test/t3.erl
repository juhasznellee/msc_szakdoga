-module(t3).
-export([t/1]).

% FROM
t(X) ->
    list_to_atom(X).

asd(Xs) ->
   [t(X) || X <- Xs]
.

megtalalni() ->
    [asd(X) || X <- lists:seq(1, 25)].

% TO
% t(X) ->
%     list_to_atom(X).

% asd(Xs) ->
%     case size_check(Xs) of
%         true -> [t(X) || X<-Xs]
%         ;
%         false -> throw("Variable criteria not met")
%     end.

% megtalalni() ->
%     [asd(X) || X <- lists:seq(1, 25)].

% size_check(X) -> length(X) < 5000000.