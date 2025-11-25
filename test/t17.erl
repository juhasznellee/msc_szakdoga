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
% This is a recursive function, there is no transformation for that.
% deny
