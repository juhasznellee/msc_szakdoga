-module(lta2).
-export([lta_func/1, outer_fun_call/1]).

% FROM
lta_func(X) ->
   list_to_atom(integer_to_list(X) ++ "example").

outer_fun_call(Arg) ->
   [lta_func(X) || X <- Arg].

% TO
% lta_func(X) ->
%    list_to_atom(integer_to_list(X) ++ "example").

% outer_fun_call(Arg) ->
%     case size_check(Arg) of
%         true -> [lta_func(X) || X<-Arg];
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 5000000.