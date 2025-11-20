-module(example4).
-export([dynamic_load/1]).

dynamic_load(Path) ->
    erl_ddll:load(Path, [])
.