-module(t4).
-export([t/1]).

% FROM
t(X) ->
   list_to_atom(X ++ "my_pool")
.

megtalalni() ->
   [t(X) ||  X <- ["1","2","3","4","5"]]
.

% TO
% t(X) ->
%    list_to_atom(X ++ "my_pool")
% .

% megtalalni() ->
%     case size_check(["1", "2", "3", "4", "5"]) of
%         true -> [t(X) || X<-["1", "2", "3", "4", "5"]]
%         ;
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 50.