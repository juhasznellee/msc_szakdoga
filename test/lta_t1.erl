-module(lta_t1).
-export([t/0]).

% FROM
t() ->
   [
      list_to_atom("my_pool" ++ integer_to_list(X)) || X <- lists:seq(1, 25)
   ].

% TO
% t() ->
%     case sanitize(lists:seq(1, 25)
%             ) of
%         true ->
%             [
%             list_to_atom("my_pool" ++ integer_to_list(X)) || X<-lists:seq(1, 25)
%             ];
%         false -> throw("Variable criteria not met")
%     end.
% sanitize(X) -> length(X) < 50.