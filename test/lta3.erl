-module(lta3).
-export([lta_func/1, outer_fun_call/1, temp/1]).

% FROM
lta_func(X) ->
   list_to_atom(integer_to_list(X) ++ "example").

outer_fun_call(Arg) ->
   [lta_func(X) || X <- temp(Arg)].

temp(Arg) ->
   lists:seq(1, 10).

% TO
% lta_func(X) ->
%    list_to_atom(integer_to_list(X) ++ "example").

% outer_fun_call(Arg) ->
%     case size_check(temp(Arg)) of
%         true -> [lta_func(X) || X<-temp(Arg)];
%         false -> throw("Variable criteria not met")
%     end.

% temp(Arg) ->
%    lists:seq(1, 10).

% size_check(X) -> length(X) < 5000000.