-module(reftr_transform_spawn_link_err_handle).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").


%%% @private
prepare(Args) ->      %Args: module, range
    App = reftr_transform_common:get_application(Args),
    Function = ?Query:exec1(App, ?Expr:function(), error),
    FunName = ?Fun:name(Function),
    case FunName of
        spawn -> transform_spawn_to_spawn_link(App);
        _ -> throw(?LocalError(no_transformation, [FunName]))
    end
.


%%% ============================================================================
%%% Transformation

%% Transforms the spawn function to spawn_link, and removes the associated link function
transform_spawn_to_spawn_link(App) ->
    [{_, SpawnAppParent}] = ?Syn:parent(App),
    ReachList = reflib_dataflow:reach(App),
    ParentsList = lists:map(fun(E) -> ?Query:exec(?Query:exec(E, ?Expr:parent()), ?Expr:parent()) end, ReachList),
    LinkApp = lists:filter(fun(E) -> E /= [] end, ParentsList),
    case LinkApp of 
        [] -> throw(?LocalErr0r(link_fun_not_found));
        _ -> ok
    end,
    LinkFunction = ?Query:exec1(LinkApp, ?Expr:function(), error),
    NameLink = ?Fun:name(LinkFunction),
    case NameLink of 
        link -> ok;
        _ -> throw(?LocalErr0r(link_fun_not_found))
    end,
    SpawnArgs = ?Query:exec1(App, ?Expr:child(2), error),
    LinkAppWithOList = hd(hd(LinkApp)),
    [{_, LinkAppParent}] = ?Syn:parent(LinkAppWithOList),
    [fun() ->
        {_ , SpawnLinkArgs} = lists:keyfind(SpawnArgs, 1, ?Syn:copy(SpawnArgs)),
        SpawnLinkAtom = ?Syn:construct({atom, spawn_link}),
        SpawnLinkApp = ?Syn:create(#expr{type = application}, [{esub, [SpawnLinkAtom]},{esub, SpawnLinkArgs}]),
        ?Syn:replace(SpawnAppParent, {node, App}, [SpawnLinkApp]),
        ?Syn:replace(LinkAppParent, {node, LinkAppWithOList}, []),
        ?Transform:touch(SpawnLinkArgs)
    end]
.


%%% ============================================================================
%%% Error messages

error_text(no_transformation, [Name]) ->
    ?MISC:format("There is no given transformation for ~p function", [Name]);
error_text(link_fun_not_found, []) ->
    ?MISC:format("No link function found related to spawn.", []).