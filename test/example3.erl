-module(example3).
-export([os_injection/1]).

os_injection(Cmd) ->
    os:cmd("less " ++ Cmd)
.