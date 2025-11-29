-module(lta_t19).
-export([fetch_config/1]).

% FROM
fetch_config(_UpdatedKeys, _ValIP, _ValGRPCPort) ->
    Keys = [key1, key2, key3],

    case Keys of
        [] ->
            ok;
        _ ->
            Req = dummy_request,
            case dummy_grpc_call(Req) of
                {ok, #dummy_resp{vars = Vars}} ->
                    [
                        begin
                            {Name, Value} = dummy_extract(Var),
                            application:set_env(miner, list_to_atom(Name), Value)
                        end
                    || #dummy_var{} = Var <- Vars
                    ],
                    ok;

                {error, Reason} ->
                    {error, Reason}
            end
    end.

% TO
% fetch_config(_UpdatedKeys, _ValIP, _ValGRPCPort) ->
%     Keys = [key1, key2, key3],

%     case Keys of
%         [] ->
%             ok;
%         _ ->
%             Req = dummy_request,
%             case dummy_grpc_call(Req) of
%                 {ok, #dummy_resp{vars = Vars}} ->
%                     case size_check(Var) of
%                         true ->
%                             [
%                             begin
%                                 {Name, Value} = dummy_extract(Var),
%                                 application:set_env(miner, list_to_atom(Name),
%                                     Value)
%                             end
%                             || #dummy_var{} = Var<-Vars
%                             ];
%                         false -> throw("Variable criteria not met")
%                     end,
%                     ok;

%                 {error, Reason} ->
%                     {error, Reason}
%             end
%     end.
% size_check(X) -> length(X) < 5000000.