-module(lta_t15).
-export([t/1]).

% FROM
t(XS) ->
   [
      list_to_atom(integer_to_list(X) ++ "my_pool") || X <- XS
   ].  
