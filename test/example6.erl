-module(example6).
-export([file_injection/1]).

file_injection(Arg) ->
    file:eval(Arg)
.