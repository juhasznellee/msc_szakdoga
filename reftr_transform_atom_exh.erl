-module(reftr_transform_atom_exh).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%%% @private
prepare(Args) ->      %Args: module, range
    App = ?Args:expr_range(Args),
    Type = ?Expr:type(hd(App)),
    case Type of
        application -> call_approp_function(hd(App));
        list_comp -> 
            [HexprClause, _ComprClause] = ?Query:exec(App,?Expr:clauses()),
            ClParent = ?Query:exec1(HexprClause, ?Clause:body(), error),
            case ?Expr:type(ClParent) of
                application -> call_approp_function(ClParent);
                infix_expr ->
                    InfixParent = ?Query:exec1(ClParent, ?Expr:parent(), error),
                    case ?Expr:type(InfixParent) of
                        application -> call_approp_function(InfixParent);
                        _ -> throw(?RefErr0r(bad_range)) 
                    end;
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        atom -> 
            AtomParent = ?Query:exec1(App, ?Expr:parent(), error),
            case ?Expr:type(AtomParent) of
                application -> call_approp_function(AtomParent);
                infix_expr ->
                    InfixParent = ?Query:exec1(AtomParent, ?Expr:parent(), error),
                    case ?Expr:type(InfixParent) of
                        application -> call_approp_function(InfixParent);
                        _ -> throw(?RefErr0r(bad_range)) 
                    end;
                _ -> throw(?RefErr0r(bad_range))
            end;
        infix_expr ->
            InfixParent = ?Query:exec1(App, ?Expr:parent(), error),
            case ?Expr:type(InfixParent) of
                application -> call_approp_function(InfixParent);
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        _ -> throw(?RefErr0r(bad_range))
    end
.

%% Calls the appropriate function depending on the functions name
call_approp_function(App) ->
    Function = ?Query:exec1(App, ?Expr:function(), error),
    Name = ?Fun:name(Function),
    ?d(Name),
    case Name of
        spawn -> transform_spawn_to_spawn_link(App);
        binary_to_atom -> transform_binary_to_existing_atom(App);
        binary_to_term -> transform_binary_to_term_safe(App);
        list_to_atom -> transform_list_to_existing_atom(App);
        list_to_existing_atom -> throw(?LocalError(already_safe, [list_to_existing_atom]));
        binary_to_existing_atom -> throw(?LocalError(already_safe, [binary_to_existing_atom]));
        _ -> ?LocalError(no_transformation, [Name])
    end
.


%%% ============================================================================
%%% Preventing atom exhaustion (Transformation)

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

%% Transforms the binary_to_atom function to binary_to_existing_atom
transform_binary_to_existing_atom(App) ->
    [{_, AppParent}] = ?Syn:parent(App),
    Args = ?Query:exec1(App, ?Expr:child(2), error),
    [fun() ->
        {_ , NewArgs} = lists:keyfind(Args, 1, ?Syn:copy(Args)),
        NewAtom = ?Syn:construct({atom, binary_to_existing_atom}),
        NewApp = ?Syn:create(#expr{type = application}, [{esub, [NewAtom]},{esub, NewArgs}]),
        ?Syn:replace(AppParent, {node, App}, [NewApp]),
        ?Transform:touch(NewArgs)
    end]
.

%% Transforms the list_to_atom function to list_to_existing_atom
transform_list_to_existing_atom(App) ->
    [{_, AppParent}] = ?Syn:parent(App),
    Args = ?Query:exec1(App, ?Expr:child(2), error),
    [fun() ->
        {_ , NewArgs} = lists:keyfind(Args, 1, ?Syn:copy(Args)),
        NewAtom = ?Syn:construct({atom, list_to_existing_atom}),
        NewApp = ?Syn:create(#expr{type = application}, [{esub, [NewAtom]},{esub, NewArgs}]),
        ?Syn:replace(AppParent, {node, App}, [NewApp]),
        ?Transform:touch(NewArgs)
    end]
.

%% Transform the binary_to_term function with adding a new 'safe' argument
transform_binary_to_term_safe(App) ->
    [{_, AppParent}] = ?Syn:parent(App),
    ArgList = ?Query:exec1(App, ?Expr:child(2), error),
    Sub = ?Query:exec(ArgList, ?Expr:deep_sub()),
    SafeAtomExists = [E || E <- Sub, ?Expr:value(E) == safe],
    case SafeAtomExists of
        [] -> ok;
        _ -> throw(?LocalError(already_safe, [binary_to_term]))
    end,
    Arg1 = ?Query:exec1(ArgList, ?Expr:child(1), error),
    [fun() ->
        FuncAtom = ?Syn:construct({atom, binary_to_term}),
        NewAtom = ?Syn:construct({atom, safe}),
        NewList = ?Syn:construct({list, [NewAtom]}),
        NewCons = ?Syn:construct({cons, [NewList]}),
        {_ , NewArg} = lists:keyfind(Arg1, 1, ?Syn:copy(Arg1)),
        NewApp = ?Syn:construct({app, FuncAtom, [NewArg, NewCons]}),
        ?Syn:replace(AppParent, {node, App}, [NewApp]),
        ?Transform:touch(NewArg)
    end]
.


%%% ============================================================================
%%% Error messages

error_text(no_transformation, [Name]) ->
    ?MISC:format("There is no given transformation for ~p function", [Name]);
error_text(link_fun_not_found, []) ->
    ?MISC:format("No link function found related to spawn.", []);
error_text(already_safe, [Name]) ->
    ?MISC:format("The ~p function is already safe.", [Name]);
error_text(replacable, []) ->
    ?MISC:format("REPLACE", []).