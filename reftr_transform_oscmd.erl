-module(reftr_transform_oscmd).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% ============================================================================
%%% Callbacks

%%% @private
prepare(Args) ->      %Args: module, range
    App = ?Args:expr_range(Args),
    File = ?Args:file(Args),
    Type = ?Expr:type(hd(App)),
    case Type of
        application -> check_arg_type(App, File);
        list_comp -> 
            [HexprClause, _ComprClause] = ?Query:exec(App,?Expr:clauses()),
            ClParent = ?Query:exec1(HexprClause, ?Clause:body(), error),
            case ?Expr:type(ClParent) of
                application -> check_arg_type(ClParent, File);
                infix_expr ->
                    InfixParent = ?Query:exec1(ClParent, ?Expr:parent(), error),
                    case ?Expr:type(InfixParent) of
                        application -> check_arg_type(InfixParent, File);
                        _ -> throw(?RefErr0r(bad_range)) 
                    end;
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        atom -> 
            AtomParent = ?Query:exec1(App, ?Expr:parent(), error),
            case ?Expr:type(AtomParent) of
                application -> check_arg_type(AtomParent, File);
                infix_expr ->
                    InfixParent = ?Query:exec1(AtomParent, ?Expr:parent(), error),
                    case ?Expr:type(InfixParent) of
                        application -> check_arg_type(InfixParent, File);
                        _ -> throw(?RefErr0r(bad_range)) 
                    end;
                _ -> throw(?RefErr0r(bad_range))
            end;
        infix_expr ->
            InfixParent = ?Query:exec1(App, ?Expr:parent(), error),
            case ?Expr:type(InfixParent) of
                application -> check_arg_type(InfixParent, File);
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        _ -> throw(?RefErr0r(bad_range))
    end
.


%%% ============================================================================
%%% Untrusted argument sanitize

%% Checks the untrusted function argumentum type
check_arg_type(App, File) ->
    ?d("----- check_arg_type -----"),
    ?d(App),
    Function = ?Query:exec1(App, ?Expr:function(), error),
    ?d(Function),
    Name = ?Fun:name(Function),
    ?d(Name),
    case Name of
        cmd ->
            Arg = ?Query:exec1(?Query:exec1(App, ?Expr:child(2), error), ?Expr:child(1), error),
            ?d(Arg),
            cmd_sanitize(App, File, Arg);
        _ -> ?LocalError(no_transformation, [Name])
    end   
.

%%% ============================================================================
%%% Sanitize

cmd_sanitize(App, File, UntrustedArg) -> 
    ?d("--- SANITIZE CMD ---"),
    [{_, AppParent}] = ?Syn:parent(App),
    [fun() ->
        %--- CRIT CHECK
        {_ , CaseSanitizeArg} = lists:keyfind(UntrustedArg, 1, ?Syn:copy(UntrustedArg)),
        CaseSanitizeArgList = ?Syn:create(#expr{type=arglist}, [{esub, CaseSanitizeArg}]),
        CaseSanitizeApp = ?Syn:create(#expr{type = application}, 
                                [{esub, [?Syn:construct({atom, variable_content_check})]}, 
                                {esub, CaseSanitizeArgList}]),

        %--- CASE - TRUE
        ThrowArgList = ?Syn:create(#expr{type=arglist}, [{esub, ?Syn:construct({string, "Variable criteria not met"})}]),
        ThrowApp = ?Syn:create(#expr{type = application}, [{esub, [?Syn:construct({atom, throw})]},{esub, ThrowArgList}]),
        TrueSanitizePattern = ?Syn:construct({pattern, [{atom, true}], [], [ThrowApp]}),

        %--- CASE - FALSE
        {_ , TrueListToAtomPart} = lists:keyfind(App, 1, ?Syn:copy(App)),
        FalseSanitizePattern = ?Syn:construct({pattern, [{atom, false}], [], [TrueListToAtomPart]}),

        %--- CASE
        NewCase = ?Syn:construct({'case', CaseSanitizeApp, [TrueSanitizePattern, FalseSanitizePattern]}),

        %--- SANITIZE FUNCTION
        SanitizeInsideAppArgList = ?Syn:create(#expr{type=arglist}, 
                                [{esub, ?Syn:construct({var, "E"})}, 
                                {esub, ?Syn:construct({string, "$@#"})}]),
        SanitizeInsideAppInfixExpr = ?Syn:construct({{infix_expr, ':'}, [{atom, lists}], [{atom, member}]}),
        SanitizeInsideApp = ?Syn:create(#expr{type = application}, 
                                [{esub, SanitizeInsideAppInfixExpr}, 
                                {esub, SanitizeInsideAppArgList}]),
        SanitizeFunExprScope = ?Syn:construct({fun_scope, [{var_pattern, "E"}], [], [SanitizeInsideApp]}),
        SanitizeFunExpr = ?Syn:construct({'fun', [SanitizeFunExprScope]}),
        SanitizeOutsideAppArgList = ?Syn:create(#expr{type=arglist}, 
                                [{esub, SanitizeFunExpr}, 
                                {esub, ?Syn:construct({var, "X"})}]),
        SanitizeOutsideAppInfixExpr = ?Syn:construct({{infix_expr, ':'}, [{atom, lists}], [{atom, any}]}),
        SanitizeOutsideApp = ?Syn:create(#expr{type = application}, 
                                [{esub, SanitizeOutsideAppInfixExpr}, 
                                {esub, SanitizeOutsideAppArgList}]),
        SanitizeFuncClause = ?Syn:construct({fun_clause, 
                                [{atom, variable_content_check}], 
                                [{var_pattern, "X"}], 
                                [], 
                                [SanitizeOutsideApp]}),
        SanitizeFuncForm = ?Syn:construct({func, [SanitizeFuncClause]}),
        
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