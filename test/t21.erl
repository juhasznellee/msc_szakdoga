-module(t21).
-export([t/1]).

% FROM


t(Options) ->
    Temp = lists:flatmap(
              fun(Key) ->
                      build_suffix(Key, maps:get(Key, Options, undefined))
              end,
              option_keys()
           ),
    list_to_atom("demo_update_" ++ Temp).

option_keys() ->
    [timeout, host, port].

build_suffix(Key, Value) ->
    io_lib:format("~p=~p;", [Key, Value]).

% TO
% No transformation for this case.
