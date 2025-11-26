-module(reftr_transform_fun_replacement).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").


%%% @private
%% Calls the appropriate function depending on the functions name
prepare(Args) ->      %Args: module, range
    App = reftr_transform_common:get_application(Args),
    Function = ?Query:exec1(App, ?Expr:function(), error),
    FunName = ?Fun:name(Function),
    case FunName of
        binary_to_atom -> transform_binary_to_existing_atom(App);
        binary_to_term -> transform_binary_to_term_safe(App);
        list_to_atom -> transform_list_to_existing_atom(App);
        list_to_existing_atom -> throw(?LocalError(already_safe, [FunName]));
        binary_to_existing_atom -> throw(?LocalError(already_safe, [FunName]));
        _ -> throw(?LocalError(no_transformation, [FunName]))
    end
.


%%% ============================================================================
%%% Transformations

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
    {InfixBool, Value} = check_is_infix_expr(App),
    ArgList = ?Query:exec1(App, ?Expr:child(2), error),
    Sub = ?Query:exec(ArgList, ?Expr:deep_sub()),
    SafeAtomExists = [E || E <- Sub, ?Expr:value(E) == safe],
    case SafeAtomExists of
        [] -> ok;
        _ -> throw(?LocalError(already_safe, [binary_to_term]))
    end,
    ArgOne = ?Query:exec1(ArgList, ?Expr:child(1), error),
    [fun() ->
        NewCons = ?Syn:construct({cons, [{list, [{atom, safe}]}]}),
        {_ , NewArg} = lists:keyfind(ArgOne, 1, ?Syn:copy(ArgOne)),
        case InfixBool of
            true ->
                NewApp = ?Syn:construct({app, {{infix_expr, ':'}, [{atom, Value}], [{atom, binary_to_term}]}, [NewArg, NewCons]});
            false -> 
                NewApp = ?Syn:construct({app, {atom, binary_to_term}, [NewArg, NewCons]})
        end,
        ?Syn:replace(AppParent, {node, App}, [NewApp]),
        ?Transform:touch(NewArg)
    end]
.


%%% ===========================================================================
%%% Helper function

check_is_infix_expr(App) ->
    Child = ?Query:exec1(App, ?Expr:child(1), error),
    case ?Expr:type(Child) of
        infix_expr -> 
            InfixLeft = ?Query:exec1(Child, ?Expr:child(1), error),
            {true, ?Expr:value(InfixLeft)};
        _ -> {false, ""}
    end
.


%%% ============================================================================
%%% Error messages

error_text(no_transformation, [Name]) ->
    ?MISC:format("There is no given transformation for ~p function", [Name]);
error_text(already_safe, [Name]) ->
    ?MISC:format("The ~p function is already safe.", [Name]).