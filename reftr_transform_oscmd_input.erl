-module(reftr_transform_oscmd_input).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% @private
prepare(Args) ->      %Args: module, range
    App = reftr_transform_common:get_application_node_from_arg(Args),
    Function = ?Query:exec1(App, ?Expr:function(), error),
    Name = ?Fun:name(Function),
    case Name of
        cmd ->
            Arg = ?Query:exec1(?Query:exec1(App, ?Expr:child(2), error), ?Expr:child(1), error),
            case ?Expr:type(Arg) of
                string -> 
                    throw(?LocalError(already_safe, []));
                _ -> cmd_input_sanitize(App, Arg)
            end;
        _ -> throw(?LocalError(no_transformation, [Name]))
    end
.


%%% ============================================================================
%%% Untrusted argument sanitize

cmd_input_sanitize(App, UntrustedArg) -> 
    File = hd(?Syn:get_file(UntrustedArg)),
    [{_, AppParent}] = ?Syn:parent(App),
    CheckFunExists = exists_check_function(File),
    [fun() ->
        %--- CRIT CHECK
        {_ , CaseSanitizeArg} = lists:keyfind(UntrustedArg, 1, ?Syn:copy(UntrustedArg)),
        CaseSanitizeArgList = ?Syn:create(#expr{type=arglist}, [{esub, CaseSanitizeArg}]),
        CaseSanitizeApp = ?Syn:create(#expr{type = application}, 
                                [{esub, [?Syn:construct({atom, check_input})]}, 
                                {esub, CaseSanitizeArgList}]),

        %--- CASE - TRUE
        ThrowArgList = ?Syn:create(#expr{type=arglist}, [{esub, ?Syn:construct({string, "Variable criteria not met"})}]),
        ThrowApp = ?Syn:create(#expr{type = application}, [{esub, [?Syn:construct({atom, throw})]},{esub, ThrowArgList}]),
        TrueSanitizePattern = ?Syn:construct({pattern, [{atom, true}], [], [ThrowApp]}),

        %--- CASE - FALSE
        {_ , FunctionPart} = lists:keyfind(App, 1, ?Syn:copy(App)),
        FalseSanitizePattern = ?Syn:construct({pattern, [{atom, false}], [], [FunctionPart]}),

        %--- CASE
        NewCase = ?Syn:construct({'case', CaseSanitizeApp, [TrueSanitizePattern, FalseSanitizePattern]}),

        %--- SANITIZE FUNCTION
        SanitizeForbiddenParams = ?Syn:construct({cons, {list, [{string, ";"}, {string, "&&"}, {string, "|"}]}}),
        FunExprScopeBodyEsub1AppInfixExpr = ?Syn:construct({{infix_expr, ':'}, [{atom, string}], [{atom, find}]}),
        FunExprScopeBodyEsub1AppArglist = ?Syn:create(#expr{type=arglist}, 
                                [{esub, ?Syn:construct({var, "X"})}, 
                                {esub, ?Syn:construct({var, "E"})}]),
        FunExprScopeBodyEsub1App = ?Syn:create(#expr{type = application}, 
                                [{esub, FunExprScopeBodyEsub1AppInfixExpr}, 
                                {esub, FunExprScopeBodyEsub1AppArglist}]),
        SanitizeOutsideFunExprScopeBody = ?Syn:construct({{infix_expr, '=/='}, [FunExprScopeBodyEsub1App], [{atom, nomatch}]}),
        SanitizeOutsideFunExprScope = ?Syn:construct({fun_scope, [{var_pattern, "E"}], [], [SanitizeOutsideFunExprScopeBody]}),
        SanitizeOutsideFunExpr = ?Syn:construct({'fun', [SanitizeOutsideFunExprScope]}),
        SanitizeOutsideAppArgList = ?Syn:create(#expr{type=arglist}, 
                                [{esub, SanitizeOutsideFunExpr}, 
                                {esub, SanitizeForbiddenParams}]),                    
        SanitizeOutsideAppInfixExpr = ?Syn:construct({{infix_expr, ':'}, [{atom, lists}], [{atom, any}]}),
        SanitizeOutsideApp = ?Syn:create(#expr{type = application}, 
                                [{esub, SanitizeOutsideAppInfixExpr}, 
                                {esub, SanitizeOutsideAppArgList}]),
        SanitizeFuncClause = ?Syn:construct({fun_clause, 
                                [{atom, check_input}], 
                                [{var_pattern, "X"}], 
                                [], 
                                [SanitizeOutsideApp]}),
        SanitizeFuncForm = ?Syn:construct({func, [SanitizeFuncClause]}),

        LastForm = ?Query:exec1(App, ?Query:seq([?Expr:clause(), ?Clause:funcl(), ?Clause:form()]), error),
        FormIndex = form_index(File, LastForm),

        ?Syn:replace(AppParent, {node, App}, [NewCase]),
        case CheckFunExists of
            true -> ok;
            false ->
                ?File:add_form(File, FormIndex + 1, SanitizeFuncForm)
        end,

        ?Transform:touch(CaseSanitizeArgList)
    end]
.


%%% ===========================================================================
%%% Helper functions

exists_check_function(File) ->
    Forms = ?Query:exec(File, ?File:forms()),
    FormFunc = lists:map(fun(E) -> ?Query:exec(E, ?Form:func()) end, Forms),
    Functions = lists:filter(fun(E) -> E /= [] end, FormFunc),
    FunNames = lists:map(fun(E) -> ?Fun:name(hd(E)) end, Functions),
    CheckFun = lists:filter(fun(E) -> E == check_input end, FunNames),
    case length(CheckFun) of
        0 -> false;
        _ -> true
    end
.

form_index(File, Form) ->
    FormFile = ?Query:exec(Form, ?Form:file()),
    case File of
        FormFile -> ?Syn:index(File, form, Form);
        _        -> length(?Query:exec(File, ?File:forms()))
    end
.


%%% ============================================================================
%%% Error messages
error_text(no_transformation, [Name]) ->
    ?MISC:format("There is no given transformation for ~p function", [Name]);
error_text(already_safe, []) ->
    ?MISC:format("No need for transformation, the function is safe.", []).