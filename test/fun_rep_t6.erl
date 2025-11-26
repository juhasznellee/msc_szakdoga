-module(fun_rep_t6).
-export([t/1]).

% FROM
t(Args) ->
    Segments = get_list(<<"name">>, Args),
    lists:map(fun binary_to_atom/1, Segments).

% TO
% No transformation for this case.
% deny