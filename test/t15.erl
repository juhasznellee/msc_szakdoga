-module(t15).
-export([t/2]).

% FROM
t(Module, Name) ->
    list_to_atom("gr_" ++ Module ++ Name).

megtalalni1(Xs) ->
    [t(X, "asd1") || X <- Xs]
.

megtalalni2(Ys) ->
    [t(Y, "asd2") || Y <- Ys]
.

% TO
% Multiple functions call the function containing list_to_atom, there is no transformation for this.
