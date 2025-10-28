-module(reftr_transform_oscmd_input).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% @private
prepare(Args) ->      %Args: module, range
    {App, File} = reftr_transform_common:get_application(Args),
    ?d(App),
    Function = ?Query:exec1(App, ?Expr:function(), error),
    ?d(Function),
    Name = ?Fun:name(Function),
    ?d(Name),
    case Name of
        cmd ->
            Arg = ?Query:exec1(?Query:exec1(App, ?Expr:child(2), error), ?Expr:child(1), error),
            ?d(Arg),
            cmd_input_sanitize(App, File, Arg);
        _ -> ?LocalError(no_transformation, [Name])
    end
.

%%% ============================================================================
%%% Untrusted argument sanitize

cmd_input_sanitize(App, File, UntrustedArg) -> 
    ?d("--- SANITIZE CMD ---"),
    [{_, AppParent}] = ?Syn:parent(App),
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
        ?d(SanitizeForbiddenParams),
        FunExprScopeBodyEsub1AppInfixExpr = ?Syn:construct({{infix_expr, ':'}, [{atom, string}], [{atom, find}]}),
        ?d(FunExprScopeBodyEsub1AppInfixExpr),
        FunExprScopeBodyEsub1AppArglist = ?Syn:create(#expr{type=arglist}, 
                                [{esub, ?Syn:construct({var, "X"})}, 
                                {esub, ?Syn:construct({var, "E"})}]),
        ?d(FunExprScopeBodyEsub1AppArglist),
        FunExprScopeBodyEsub1App = ?Syn:create(#expr{type = application}, 
                                [{esub, FunExprScopeBodyEsub1AppInfixExpr}, 
                                {esub, FunExprScopeBodyEsub1AppArglist}]),
        ?d(FunExprScopeBodyEsub1App),
        SanitizeOutsideFunExprScopeBody = ?Syn:construct({{infix_expr, '=/='}, [FunExprScopeBodyEsub1App], [{atom, nomatch}]}),
        ?d(SanitizeOutsideFunExprScopeBody),
        SanitizeOutsideFunExprScope = ?Syn:construct({fun_scope, [{var_pattern, "E"}], [], [SanitizeOutsideFunExprScopeBody]}),
        ?d(SanitizeOutsideFunExprScope),
        SanitizeOutsideFunExpr = ?Syn:construct({'fun', [SanitizeOutsideFunExprScope]}),
        ?d(SanitizeOutsideFunExpr),
        SanitizeOutsideAppArgList = ?Syn:create(#expr{type=arglist}, 
                                [{esub, SanitizeOutsideFunExpr}, 
                                {esub, SanitizeForbiddenParams}]),
        ?d(SanitizeOutsideAppArgList),                        
        SanitizeOutsideAppInfixExpr = ?Syn:construct({{infix_expr, ':'}, [{atom, lists}], [{atom, any}]}),
        ?d(SanitizeOutsideAppInfixExpr),
        SanitizeOutsideApp = ?Syn:create(#expr{type = application}, 
                                [{esub, SanitizeOutsideAppInfixExpr}, 
                                {esub, SanitizeOutsideAppArgList}]),
        ?d(SanitizeOutsideApp),
        SanitizeFuncClause = ?Syn:construct({fun_clause, 
                                [{atom, check_input}], 
                                [{var_pattern, "X"}], 
                                [], 
                                [SanitizeOutsideApp]}),
        ?d(SanitizeFuncClause),
        SanitizeFuncForm = ?Syn:construct({func, [SanitizeFuncClause]}),
        ?d(SanitizeFuncForm),
       
        LastForm = ?Query:exec1(App, ?Query:seq([?Expr:clause(), ?Clause:funcl(), ?Clause:form()]), error),
        FormIndex = form_index(File, LastForm),
        ?Syn:replace(AppParent, {node, App}, [NewCase]),
        ?File:add_form(File, FormIndex + 1, SanitizeFuncForm),
        
        ?Transform:touch(CaseSanitizeArgList)
    end]
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
error_text(link_fun_not_found, []) ->
    ?MISC:format("No link function found related to spawn.", []);
error_text(already_safe, [Name]) ->
    ?MISC:format("The ~p function is already safe.", [Name]);
error_text(no_variable, []) ->
    ?MISC:format("No matching variable.");
error_text(criteria_not_met, []) ->
    ?MISC:format("Variable criteria not met.", []);
error_text(replacable, []) ->
    ?MISC:format("REPLACE", []).