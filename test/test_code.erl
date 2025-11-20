-module(test_code).
-export([test/1]).

test(Arg) -> 
    io:format("Hello World! " ++ Arg).
