-module(lta_t14).
-export([t/0]).

% FROM
t() ->
[
  lists:foldr(
    fun(Str, Acc) ->
      [list_to_atom(Str) | Acc]
    end,
    [],
    [integer_to_list(X) || X <- lists:seq(1, 25)]
  )
].

% TO
% t() ->
% [
%     case sanitize(lists:seq(1, 25)) of
%         true ->
%             lists:foldr(
%                 fun(Str, Acc) ->
%                     [list_to_atom(Str) | Acc]
%                 end,
%                 [],
%                 [integer_to_list(X) || X<-lists:seq(1, 25)]
%                 )
%         ;
%         false -> throw("Variable criteria not met")
%     end].
% sanitize(X) -> length(X) < 50.