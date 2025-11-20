-module(oscmd_t2).
-export([t/0]).

% FROM
t() ->
   os:cmd("less").

% TO
% "No need for transformation, the function is safe."
