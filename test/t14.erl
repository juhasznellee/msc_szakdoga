-module(t14).
-export([set_config_value/2]).

% FROM
set_config_value(Value, string) -> Value;
set_config_value(Value, binary) -> list_to_binary(Value);
set_config_value(Value, atom) -> list_to_atom(Value);
set_config_value(Value, integer) -> list_to_integer(Value).

cache_os_envvar(Var, Keys, Type) ->
    [case os:getenv(Key) of
        false -> ok;
        Value -> set_config(Var, set_config_value(Value, Type))
    end || Key <- Keys]
.


% TO
% set_config_value(Value, string) -> Value;
% set_config_value(Value, binary) -> list_to_binary(Value);
% set_config_value(Value, atom) -> list_to_atom(Value);
% set_config_value(Value, integer) -> list_to_integer(Value).

% cache_os_envvar(Var, Keys, Type) ->
%     case size_check(Keys) of
%         true ->
%             [case os:getenv(Key) of
%                 false -> ok;
%                 Value -> set_config(Var, set_config_value(Value, Type))
%             end || Key<-Keys];
%         false -> throw("Variable criteria not met")
%     end.

% size_check(X) -> length(X) < 10000.