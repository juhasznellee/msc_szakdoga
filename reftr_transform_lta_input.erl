-module(reftr_transform_lta_input).
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
        list_to_atom ->
            Arg = ?Query:exec1(?Query:exec1(App, ?Expr:child(2), error), ?Expr:child(1), error),
            ?d(Arg),
            ?d(?Expr:type(Arg)),
            case ?Expr:type(Arg) of 
                infix_expr -> 
                   case_of_infix_expr_arg(App, File, Arg);
                application -> % t2.erl | t6.erl | t7.erl | t8.erl | t9.erl | t10.erl | t11.erl | t12.erl
                    case_of_application_arg(App, File, Arg);
                variable -> % t3.erl | t5.erl | t13.erl | t14.erl
                    case_of_variable_arg(App, File, Arg);
                string -> 
                    throw(?LocalError(no_multi_atom_gen, []));
                integer ->
                    throw(?LocalError(no_multi_atom_gen, []));
                _ -> throw(?LocalError(replacable, []))
            end;
        list_to_existing_atom -> ?LocalError(already_safe, [Name]);
        _ -> ?LocalError(no_transformation, [Name])
    end   
.


%%% ============================================================================
%%% Untrusted argument sanitize


% t1.erl | t4.erl 
%% If the untrusted argument is an infix expression
case_of_infix_expr_arg(App, File, Arg) ->
    ?d("----- case_of_infix_expr_arg -----"),
    Ch1 = ?Query:exec1(Arg, ?Expr:child(1), error),
    ?d(Ch1),
    Ch2 = ?Query:exec1(Arg, ?Expr:child(2), error),
    ?d(Ch2),
    ?d(?Expr:type(Ch1)),
    ?d(?Expr:type(Ch2)),
    case lists:member(?Expr:type(Ch1), [application, cons, variable, infix_expr]) of
        true -> case_of_infix_expr_child(App, File, Ch1);
        false -> 
            case lists:member(?Expr:type(Ch2), [application, cons, variable, infix_expr]) of
                true -> case_of_infix_expr_child(App, File, Ch2);
                false -> throw(?LocalError(replacable, []))
            end
    end
.

case_of_infix_expr_child(App, File, Child) ->
    case ?Expr:type(Child) of
        application -> % t1.erl
            ChildAppArg = ?Query:exec1(?Query:exec1(Child, ?Expr:child(2), error), ?Expr:child(1), error),
            ?d(ChildAppArg),
            case ?Expr:type(ChildAppArg) of
                variable -> 
                    handle_variable_inside_application(App, File, ChildAppArg);
                _ -> throw(?LocalError(replacable, []))
            end;
        cons ->
            ListCompApp = get_list_comp_part(App),
            list_to_atom_sanitize(ListCompApp, File, Child);        
        variable -> % t4.erl
            case_of_variable_arg(App, File, Child);
        infix_expr ->
            case_of_infix_expr_arg(App, File, Child);
        _ -> ok
    end
.

% t2.erl | t6.erl | t7.erl | t8.erl | t9.erl | t10.erl | t11.erl | t12.erl
%% If the untrusted argument is an application
case_of_application_arg(App, File, Arg) ->
    ?d("----- case_of_application_arg -----"),
    ChOfArg = ?Query:exec1(Arg, ?Expr:child(2), error),
    ?d(ChOfArg),
    case check_children_number(ChOfArg) of    
        1 -> % --------------------- t2.erl | t8.erl | t10.erl | t11.erl ---------------------
            Ch1 = ?Query:exec1(ChOfArg, ?Expr:child(1), error),
            ?d(Ch1),
            case_of_application_child(App, File, Ch1);
        2 -> % --------------------- t6.erl | t7.erl | t9.erl | t12.erl ---------------------
            Ch1 = ?Query:exec1(?Query:exec1(Arg, ?Expr:child(2), error), ?Expr:child(1), error),
            ?d(Ch1),
            Ch2 = ?Query:exec1(?Query:exec1(Arg, ?Expr:child(2), error), ?Expr:child(2), error),
            ?d(Ch2),
            case lists:member(?Expr:type(Ch1), [application, parenthesis, variable]) of
                true -> case_of_application_child(App, File, Ch1);
                false -> 
                    case lists:member(?Expr:type(Ch2), [application, parenthesis, variable]) of
                        true -> case_of_application_child(App, File, Ch2);
                        false -> throw(?LocalError(replacable, []))
                    end
            end;
        _ -> throw(?LocalError(replacable, []))
    end
.

case_of_application_child(App, File, Child) ->
    ?d(?Expr:type(Child)),
    case ?Expr:type(Child) of
        variable -> 
            handle_variable_inside_application(App, File, Child);
        application -> 
            case_of_application_arg(App, File, Child);
        parenthesis -> % Plusz zárójel, de csak 1 arg - annál több nem lehet, hiba
            case check_children_number(Child) of
                1 -> 
                    NextChild = ?Query:exec1(Child, ?Expr:child(1), error),
                    handle_variable_inside_application(App, File, NextChild);
                _ -> throw(?LocalError(replacable, [])) 
            end;
        _ -> throw(?LocalError(replacable, []))    
    end
.

% t2.erl | t6.erl | t7.erl | t8.erl | t9.erl | t10.erl | t11.erl | t12.erl | t1.erl
%% If the untrusted argument is a variable inside an application
handle_variable_inside_application(App, File, Child) ->
    ?d("----- handle_variable_inside_application -----"),
    ?d(?Expr:role(Child)),
    case ?Expr:role(Child) of
        expr ->
            {Length, DepsOrFlow} = get_flow_deps(Child),   % if Length = 0 -> Flow | if Lenght = 1 -> Deps
            case Length of
                1 ->
                    case ?Expr:role(DepsOrFlow) of
                        expr -> 
                            ListCompApp = get_list_comp_part(App),
                            list_to_atom_sanitize(ListCompApp, File, DepsOrFlow);
                        pattern -> find_var_called_by_func(DepsOrFlow, File, App);
                        _ -> throw(?LocalError(replacable, []))
                    end;
                0 -> % rekurzív külső hívás - t16.erl | lta_t16
                    find_var_called_by_func(DepsOrFlow, File, App);
                _ -> throw(?LocalError(replacable, []))
            end;
        _ -> throw(?LocalError(replacable, []))
    end
.


% t3.erl | t5.erl | t13.erl | t14.erl 
%% If the untrusted argument is a variable
case_of_variable_arg(App, File, Arg) ->
    ?d("----- case_of_variable_arg -----"),
    {Length, DepsOrFlow} = get_flow_deps(Arg),  % if Length = 0 -> Flow | if Lenght = 1 -> Dep
    case Length of
        1 -> 
            ?d(?Expr:type(DepsOrFlow)),
            ?d(?Expr:role(DepsOrFlow)),
            case ?Expr:type(DepsOrFlow) of
                variable ->
                    case ?Expr:role(DepsOrFlow) of
                        expr -> % --------------------- t3.erl ---------------------
                                ListCompApp = get_list_comp_part(App),
                                list_to_atom_sanitize(ListCompApp, File, DepsOrFlow);
                        _ -> throw(?LocalError(replacable, []))
                    end;
                tuple ->
                    case ?Expr:role(DepsOrFlow) of
                        pattern -> % --------------------- t5.erl ---------------------
                            ExprClause = ?Query:exec1(DepsOrFlow, ?Expr:clause(), error),
                            ?d(ExprClause),
                            ?d(?Clause:type(ExprClause)),
                            case ?Clause:type(ExprClause) of
                                funexpr ->
                                    FunExpr = ?Query:exec1(ExprClause, ?Clause:clauseof(), error),
                                    ?d(FunExpr),
                                    ?d(?Expr:type(FunExpr)),
                                    case ?Expr:type(FunExpr) of
                                        fun_expr ->
                                            ParentOfFunExpr = ?Query:exec1(FunExpr, ?Expr:parent(), error),
                                            ?d(ParentOfFunExpr),
                                            MapAppParent = ?Query:exec1(ParentOfFunExpr, ?Expr:parent(), error),
                                            ?d(MapAppParent),
                                            FunName = ?Expr:value(?Query:exec1(?Query:exec1(MapAppParent, ?Expr:child(1), error), ?Expr:child(2), error)),
                                            ?d(FunName),
                                            case FunName of
                                                map ->
                                                    Child2 = ?Query:exec1(ParentOfFunExpr, ?Expr:child(2), error),  % UntrustedArg
                                                    ?d(Child2),
                                                    list_to_atom_sanitize(MapAppParent, File, Child2);
                                                _ -> throw(?LocalError(replacable, []))
                                            end;
                                        _ -> throw(?LocalError(replacable, []))
                                    end;
                                fundef -> find_var_called_by_func(ExprClause, File, App); %throw(?LocalError(recursive, []));
                                _ -> throw(?LocalError(replacable, []))
                            end;
                        _ -> throw(?LocalError(replacable, []))    
                    end;
                cons -> % --------------------- t4.erl ---------------------
                    ListCompApp = get_list_comp_part(App),
                    list_to_atom_sanitize(ListCompApp, File, DepsOrFlow)
            end;
        0 -> % --------------------- t13.erl | t14.erl | t16.erl ---------------------
            ?d(?Expr:type(DepsOrFlow)),
            ?d(?Expr:role(DepsOrFlow)),
            case ?Expr:role(DepsOrFlow) of
                pattern ->
                    ExprClause = ?Query:exec1(DepsOrFlow, ?Expr:clause(), error),
                    ?d(ExprClause),
                    ?d(?Clause:type(ExprClause)),
                    case ?Clause:type(ExprClause) of
                        funexpr ->
                            FunExpr = ?Query:exec1(ExprClause, ?Clause:clauseof(), error),
                            ?d(FunExpr),
                            ?d(?Expr:type(FunExpr)),
                            case ?Expr:type(FunExpr) of
                                fun_expr ->
                                    ParentOfFunExpr = ?Query:exec1(FunExpr, ?Expr:parent(), error),
                                    ?d(ParentOfFunExpr),
                                    FoldlAppParent = ?Query:exec1(ParentOfFunExpr, ?Expr:parent(), error),
                                    ?d(FoldlAppParent),
                                    FunName = ?Expr:value(?Query:exec1(?Query:exec1(FoldlAppParent, ?Expr:child(1), error), ?Expr:child(2), error)),
                                    ?d(FunName),
                                    case lists:member(FunName, [foldl, foldr]) of
                                        true ->
                                            Child3 = ?Query:exec1(ParentOfFunExpr, ?Expr:child(3), error),  % UntrustedArg
                                            ?d(Child3),
                                            ?d(?Expr:type(Child3)),
                                            case ?Expr:type(Child3) of
                                                variable ->
                                                    handle_variable_inside_application(FoldlAppParent, File, Child3);
                                                list_comp ->
                                                    [HexprClause, _ComprClause] = ?Query:exec(Child3,?Expr:clauses()),
                                                    ?d(HexprClause),
                                                    Child3ArgApp = ?Query:exec1(HexprClause, ?Clause:body(), error),
                                                    ?d(Child3ArgApp),
                                                    ?d(?Expr:type(Child3ArgApp)),
                                                    case_of_application_arg(FoldlAppParent, File, Child3ArgApp);
                                                _ -> list_to_atom_sanitize(FoldlAppParent, File, Child3)
                                            end;
                                        false -> throw(?LocalError(replacable, []))
                                    end;
                                _ -> throw(?LocalError(replacable, []))
                            end;
                        fundef -> find_var_called_by_func(DepsOrFlow, File, App); %throw(?LocalError(recursive, []));  % t15.erl | lta_t15.erl
                        pattern ->
                            PatternFlow = ?Query:exec1(DepsOrFlow, reflib_dataflow:flow_back(), error),
                            ?d(PatternFlow),
                            case ?Expr:type(PatternFlow) of
                                application ->
                                    ChOfArg = ?Query:exec1(PatternFlow, ?Expr:child(2), error),
                                    ?d(ChOfArg),
                                    case check_children_number(ChOfArg) of
                                        1 ->
                                            Ch1 = ?Query:exec1(?Query:exec1(PatternFlow, ?Expr:child(2), error), ?Expr:child(1), error),
                                            case lists:member(?Expr:type(Ch1), [atom, string, integer]) of
                                                true ->
                                                    throw(?LocalError(no_multi_atom_gen, []));
                                                false -> 
                                                    throw(?LocalError(replacable, []))
                                            end;
                                        2 ->
                                            Ch1 = ?Query:exec1(?Query:exec1(PatternFlow, ?Expr:child(2), error), ?Expr:child(1), error),
                                            ?d(Ch1),
                                            Ch2 = ?Query:exec1(?Query:exec1(PatternFlow, ?Expr:child(2), error), ?Expr:child(2), error),
                                            ?d(Ch2),
                                            case lists:member(?Expr:type(Ch1), [atom, string, integer]) of
                                                true -> throw(?LocalError(no_multi_atom_gen, []));
                                                false -> 
                                                    case lists:member(?Expr:type(Ch2), [atom, string, integer]) of
                                                        true -> throw(?LocalError(no_multi_atom_gen, []));
                                                        false -> throw(?LocalError(replacable, []))
                                                    end
                                            end;
                                        _ -> throw(?LocalError(replacable, []))
                                    end;
                                atom -> throw(?LocalError(no_multi_atom_gen, []));
                                _ -> throw(?LocalError(replacable, []))
                            end;
                        _ -> throw(?LocalError(replacable, []))
                    end;
                _ -> throw(?LocalError(replacable, []))
            end;
        _ -> throw(?LocalError(replacable, []))
    end
.

find_var_called_by_func(Flow, File, App) ->
    ?d(?Expr:type(Flow)),
    NewFunFlow = ?Query:exec1(Flow, reflib_dataflow:flow_back(), error),
    ?d(NewFunFlow),
    NewFunVar =  ?Query:exec(NewFunFlow, [{call, back}]),
    ?d(NewFunVar),
    
    case length(NewFunVar) of
        0 -> % t14.erl
            case ?Expr:type(NewFunFlow) of
                application -> 
                    ListGen = ?Query:exec1(?Query:exec1(NewFunFlow, ?Expr:parent(), error), ?Expr:parent(), error),
                    ?d(ListGen),
                    NewListCompApp = get_list_comp_part(ListGen),
                    ?d(NewListCompApp),
                    [_HexprClause, ComprClause] = ?Query:exec(ListGen,?Expr:clauses()),
                    ?d(ComprClause),
                    Untrusted = ?Query:exec1(ComprClause, ?Clause:body(), error),
                    ?d(Untrusted),
                    list_to_atom_sanitize(NewListCompApp, File, Untrusted);
                _ -> throw(?LocalError(no_fun_call, []))
            end;
        1 -> % t16.erl | t17.erl
            NewFunVarWithOList = hd(NewFunVar),
            case ?Expr:type(NewFunVarWithOList) of
                variable ->
                    NewApp = ?Query:exec1(?Query:exec1(NewFunVarWithOList, ?Expr:parent(), error), ?Expr:parent(), error),
                    ?d(NewApp),
                    {NewLength, NewDepsOrFlow} = get_flow_deps(NewFunVarWithOList),
                    ?d(NewDepsOrFlow),
                    case NewLength of
                        1 ->
                            case ?Expr:role(NewDepsOrFlow) of
                                expr ->
                                    NewListCompApp = get_list_comp_part(NewApp),
                                    list_to_atom_sanitize(NewListCompApp, File, NewDepsOrFlow); %t16.erl
                                _ -> list_to_atom_sanitize(NewApp, File, NewDepsOrFlow)
                            end;
                        0 -> find_var_called_by_func(NewDepsOrFlow, File, App) %lta_t16.erl | t14.erl
                    end;
                _ -> throw(?LocalError(replacable, []))
            end;
        _ ->
            NewFunVarApp = lists:filter(fun(E) -> ?Expr:type(E) == application end, NewFunVar),
            ?d(NewFunVarApp),
            case length(NewFunVarApp) of
                1 -> 
                    FunCall = ?Query:exec1(?Query:exec1(hd(NewFunVarApp), ?Expr:parent(), error), ?Expr:parent(), error),
                    ?d(FunCall),
                    list_to_atom_sanitize(FunCall, File, hd(NewFunVarApp)); % t17.erl
                _ -> throw(?LocalError(multi_fun_call, [])) % t15.erl
            end
    end
.

get_flow_deps(Arg) ->
    Flow = ?Query:exec1(Arg, reflib_dataflow:flow_back(), error),
    ?d(Flow),
    DepsList = reflib_dataflow:deps(Flow),
    ?d(DepsList),
    case length(DepsList) of
        1 -> {1, hd(DepsList)};
        0 -> {0, Flow}
    end
.

get_list_comp_part(App) ->
    [{_, AppParent}] = ?Syn:parent(App),
    ?d(AppParent),
    case ?Clause:is_clause(AppParent) of
        true ->
            ListComp = ?Query:exec(AppParent, ?Clause:clauseof()),
            ?d(ListComp),
            case length(ListComp) of
                1 ->
                    ListCompWithOList = hd(ListComp),
                    case ?Expr:type(ListCompWithOList) of
                        list_comp -> ListCompWithOList;
                        _ -> App
                    end;
                _ -> App
            end;
        _ -> App
    end
.

%%% ============================================================================
%%% Sanitize

list_to_atom_sanitize(App, File, UntrustedArg) -> % több transzforom map vagy lc
    ?d("--- SANITIZE LIST_TO_ATOM ---"),
    [{_, AppParent}] = ?Syn:parent(App),
    CheckFunExists = exists_check_function(File),
    [fun() ->
        %--- CRIT CHECK
        {_ , CaseSanitizeArg} = lists:keyfind(UntrustedArg, 1, ?Syn:copy(UntrustedArg)),
        CaseSanitizeArgList = ?Syn:create(#expr{type=arglist}, [{esub, CaseSanitizeArg}]),
        CaseSanitizeApp = ?Syn:create(#expr{type = application}, 
                                [{esub, [?Syn:construct({atom, size_check})]}, 
                                {esub, CaseSanitizeArgList}]),

        %--- CASE - TRUE
        {_ , FunctionPart} = lists:keyfind(App, 1, ?Syn:copy(App)),
        TrueSanitizePattern = ?Syn:construct({pattern, [{atom, true}], [], [FunctionPart]}),

        %--- CASE - FALSE
        ThrowArgList = ?Syn:create(#expr{type=arglist}, [{esub, ?Syn:construct({string, "Variable criteria not met"})}]),
        ThrowApp = ?Syn:create(#expr{type = application}, [{esub, [?Syn:construct({atom, throw})]},{esub, ThrowArgList}]),
        FalseSanitizePattern = ?Syn:construct({pattern, [{atom, false}], [], [ThrowApp]}),

        %--- CASE
        NewCase = ?Syn:construct({'case', CaseSanitizeApp, [TrueSanitizePattern, FalseSanitizePattern]}),

        %--- SANITIZE FUNCTION
        SanitizeFuncLeftArgList = ?Syn:create(#expr{type=arglist}, [{esub, ?Syn:construct({var, "X"})}]),
        SanitizeFuncLeft = ?Syn:create(#expr{type = application}, 
                                [{esub, [?Syn:construct({atom, length})]}, 
                                {esub, SanitizeFuncLeftArgList}]),
        SanitizeFuncRight = ?Syn:construct({integer, 10000}),
        SanitizeFuncClause = ?Syn:construct({fun_clause, 
                                [{atom, size_check}], 
                                [{var_pattern, "X"}], 
                                [], 
                                [{{infix_expr, '<'}, SanitizeFuncLeft, SanitizeFuncRight}]}),
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

exists_check_function(File) ->
    Forms = ?Query:exec(File, ?File:forms()),
    FormFunc = lists:map(fun(E) -> ?Query:exec(E, ?Form:func()) end, Forms),
    Functions = lists:filter(fun(E) -> E /= [] end, FormFunc),
    FunNames = lists:map(fun(E) -> ?Fun:name(hd(E)) end, Functions),
    CheckFun = lists:filter(fun(E) -> E == size_check end, FunNames),
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
%%% Checks

check_children_number(Node) ->
    List = ?Query:exec(Node, ?Expr:children()),
    length(List)
.


%%% ============================================================================
%%% Error messages

error_text(no_transformation, [Name]) ->
    ?MISC:format("There is no given transformation for ~p function", [Name]);
error_text(no_fun_call, []) ->
    ?MISC:format("Function call not found.", []);
error_text(multi_fun_call, []) ->
    ?MISC:format("Multiple function called, there is no transformation.", []);
error_text(no_multi_atom_gen, []) ->
    ?MISC:format("No multiple atom generation.", []);
error_text(already_safe, [Name]) ->
    ?MISC:format("The ~p function is already safe.", [Name]);
error_text(recursive, []) ->
    ?MISC:format("This is a recursive function, there is no transformation.", []);
error_text(replacable, []) ->
    ?MISC:format("REPLACE", []).