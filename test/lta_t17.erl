-module(lta_t17).
-export([t/1]).

% FROM
t(Module) ->
    list_to_atom(Module).


megtalalni(Xs) ->
    [t(X) || X <- Xs].

% TO
% t(Module) ->
%     list_to_atom(Module).


% megtalalni(Xs) ->
%     case size_check(Xs) of
%         true -> [t(X) || X<-Xs]
%         ;
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 5000000.