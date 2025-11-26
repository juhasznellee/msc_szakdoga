-module(lta_t18).
-export([t/0]).

% FROM
t() ->
    K = '$$test$$mochiglobal',
    delete(K).

delete(K) ->
    delete(K, key_to_module(K)).

delete(_K, Mod) ->
    ok.

key_to_module(K) ->
    io:format("Asd: ~p~n", [K]),
    list_to_atom("mochiglobal:" ++ atom_to_list(K)).
	
% TO
% No transformation is needed because there is no multiple atom generation.
