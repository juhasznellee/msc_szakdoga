-module(t13).
-export([t/1]).

% FROM
t(XS) ->
   case config(cookie) of
        "" -> ok;
        Cookie -> erlang:set_cookie(node(), list_to_atom(Cookie))
    end.

config(Key) when is_atom(Key) ->
    case temp(Key) of
        _ -> ok;
        {ok, Val} -> Val
    end.

temp(ASD) -> ASD.

% TO
