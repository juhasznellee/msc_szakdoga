-module(t18).
-export([t/0]).

% FROM
pp_arguments(PF, As, I) ->
    case {As, io_lib:printable_list(As)} of
        {[Int | T], true} ->
            L = integer_to_list(Int),
            Ll = length(L),
            A = list_to_atom(lists:duplicate(Ll, $a)),
            io:format("Alma: ~p~n", [A]);
        _ -> 
            io:format("Asd: ~p~n")
    end.

t() ->
    pp_arguments(ok, [49,50,51], 0)
.

% TO
% No transformation for this case.