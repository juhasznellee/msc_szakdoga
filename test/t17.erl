-module(t17).
-export([t/0]).

% FROM
y([X|Xs]) ->
   P = list_to_atom(integer_to_list(X)),
   io:format("Adat: ~p~n", [P]),
   y(Xs);
y([]) ->
    ok.

t() ->
    y(lists:seq(1, 10)).

% TO
% y([X|Xs]) ->
%    P = list_to_atom(integer_to_list(X)),
%    io:format("Adat: ~p~n", [P]),
%    y(Xs);
% y([]) ->
%     ok.

% t() ->
%     case size_check(lists:seq(1, 10)) of
%         true -> y(lists:seq(1, 10));
%         false -> throw("Variable criteria not met")
%     end.
% size_check(X) -> length(X) < 10000.