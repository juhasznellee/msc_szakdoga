-module(lta_t13).
-export([t/0]).

% FROM
t() ->
[
   lists:foldl(
      fun(Str, Acc) ->
          [list_to_atom(Str) | Acc]
      end,
      [],
      XS
    ) || XS <- lists:seq(1, 25)].

% TO
% t() ->
%     case size_check(lists:seq(1, 25)) of
%         true ->
%             [
%             lists:foldl(
%                 fun(Str, Acc) ->
%                     [list_to_atom(Str) | Acc]
%                 end,
%                 [],
%                 XS
%                 ) || XS<-lists:seq(1, 25)];
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 5000000.