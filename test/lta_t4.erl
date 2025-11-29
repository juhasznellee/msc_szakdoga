-module(lta_t4).
-export([t/0]).

% FROM
t() ->
    [
      list_to_atom(X ++ "my_pool") || X <- ["1","2","3","4","5"]
    ].

% TO
% t() ->
%     case size_check(["1", "2", "3", "4", "5"]
%             ) of
%         true ->
%             [
%             list_to_atom(X ++ "my_pool") || X<-["1", "2", "3", "4", "5"]
%             ];
%         false -> throw("Variable criteria not met")
%     end.
% size_check(X) -> length(X) < 5000000.