-module(t10).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(lists:append(XS, "ASD")).

megtalalni() ->
   [t(X) || X <- lists:seq(1, 25)]
.

% TO
% t(XS) ->
%    list_to_atom(lists:append(XS, "ASD")).

% megtalalni() ->
%     case size_check(lists:seq(1, 25)) of
%         true -> [t(X) || X<-lists:seq(1, 25)]
%         ;
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 10000.