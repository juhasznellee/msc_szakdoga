-module(lta_t21).
-export([fetch_config/1]).

% FROM

megtalalni() ->
    [lta_t20:t(X) || X <- lists:seq(1, 25)].

% TO
% megtalalni() ->
%     case size_check(lists:seq(1, 25)) of
%         true -> [lta_t20:t(X) || X<-lists:seq(1, 25)];
%         false -> throw("Variable criteria not met")
%     end.

% % TO
% size_check(X) -> length(X) < 10000.