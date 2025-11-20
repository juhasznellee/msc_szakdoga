-module(example5).
-export([nif_load/1]).

nif_load(Path) ->
    erlang:load_nif(Path, 0)
.