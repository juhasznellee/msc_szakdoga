-module(lta_t6).
-export([t/0]).

% FROM
t() ->
    [
        list_to_atom(filename:basename(XS, ".erl")) || XS <- lists:seq(1, 25)
    ].


% TO
% t() ->
%     case size_check(lists:seq(1, 25)
%             ) of
%         true ->
%             [
%             list_to_atom(filename:basename(XS, ".erl")) || XS<-lists:seq(1, 25)
%             ];
%         false -> throw("Variable criteria not met")
%     end.
% sansize_checkitize(X) -> length(X) < 50.