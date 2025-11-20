-module(example7).
-export([dynamic_injection/1]).

dynamic_injection(Arg) ->
    File = "./test_code.erl",
    file:write_file(File, Arg),
    compile:file(File)
.