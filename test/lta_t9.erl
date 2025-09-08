-module(lta_t9).
-export([t/0]).

% FROM
t() ->
    [
        list_to_atom(filename:basename("teszt - ", XS)) || XS <- lists:seq(1, 25)
    ].

% TO
% t() ->
%     case sanitize(lists:seq(1, 25)
%             ) of
%         true ->
%             [
%             list_to_atom(filename:basename("teszt - ", XS)) ||
%             XS<-lists:seq(1, 25)
%             ];
%         false -> throw("Variable criteria not met")
%     end.
% sanitize(X) -> length(X) < 50.