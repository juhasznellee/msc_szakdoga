-module(t12).
-export([t/1]).

% FROM
t(XS) ->
   list_to_atom(filename:rootname(filename:basename(XS, "asdASD"))).

megtalalni() ->
   [t(X) || X <- lists:seq(1, 25)]
.

% TO
% t(XS) ->
%    list_to_atom(filename:rootname(filename:basename(XS, "asdASD"))).

% megtalalni() ->
%     case size_check(lists:seq(1, 25)) of
%         true -> [t(X) || X<-lists:seq(1, 25)]
%         ;
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 50.
