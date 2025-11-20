-module(example1).
-export([unsafe_port/1]).

unsafe_port(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, exit_status]),
    receive
        {Port, {data, Data}} ->
            io:format("Kapott adat: ~p~n", [Data])
    end
.