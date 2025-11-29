-module(lta_t16).
-export([t/1]).

% FROM
t(Module) ->
    list_to_atom("gr_" ++ atom_to_list(Module)).

asd(Y) ->
   t(Y)
.

megtalalni(Xs) ->
   [asd(X) || X <- Xs]
.

% TO
% t(Module) ->
%     list_to_atom("gr_" ++ atom_to_list(Module)).

% asd(Y) ->
%    t(Y)
% .

% megtalalni(Xs) ->
%     case size_check(Xs) of
%         true -> [asd(X) || X<-Xs]
%         ;
%         false -> throw("Variable criteria not met")
%     end.
% size_check(X) -> length(X) < 5000000.