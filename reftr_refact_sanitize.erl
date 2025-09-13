-module(reftr_refact_sanitize).
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
            ok;
        integer ->
            ok;
        _ -> throw(?LocalError(replacable, []))
    end
.

% t1.erl | t15.erl |
%% If the untrusted argument is an infix expression
case_of_infix_expr_arg(App, File, Arg) ->
    ?d("----- case_of_infix_expr_arg -----"),
    Ch1 = ?Query:exec1(Arg, ?Expr:child(1), error),
    ?d(Ch1),
    Ch2 = ?Query:exec1(Arg, ?Expr:child(2), error),
    ?d(Ch2),
    ?d(?Expr:type(Ch1)),
    ?d(?Expr:type(Ch2)),
    case lists:member(?Expr:type(Ch1), [application, cons, variable]) of
        true -> case_of_infix_expr_child(App, File, Ch1);
        false -> ok
    end
    %?d("-- CH2 --")
    % case lists:member(?Expr:type(Ch2), [application, cons, variable]) of
    %     true -> case_of_infix_expr_child(App, File, Ch2);
    %     false -> ok
    % end
.

case_of_infix_expr_child(App, File, Child) ->
    case ?Expr:type(Child) of
        application -> 
            ChildAppArg = ?Query:exec1(?Query:exec1(Child, ?Expr:child(2), error), ?Expr:child(1), error),
            ?d(ChildAppArg),
            case ?Expr:type(ChildAppArg) of
                variable -> 
                    handle_variable_inside_application(App, File, ChildAppArg);
                _ -> throw(?LocalError(replacable, []))
            end
        ;
        cons -> 
            case check_children_number(Child) of
                1 -> ok;
                _ -> throw(?LocalError(replacable, []))
            end,
            DoubleCheckChild = ?Query:exec1(Child, ?Expr:child(1), error),
            ?d(DoubleCheckChild),
            ?d(?Expr:type(DoubleCheckChild)),
            case ?Expr:type(DoubleCheckChild) of
                list -> ok;
                _ -> throw(?LocalError(replacable, []))
            end,
            case check_children_number(DoubleCheckChild) of
                1 -> ok;
                _ -> throw(?LocalError(replacable, []))
            end,
            ConsChild = ?Query:exec1(DoubleCheckChild, ?Expr:child(1), error),     % UntrustedArg
            ?d(ConsChild),
            list_to_atom_sanitize(App, File, ConsChild);
        variable -> %??????
            % Ch2VarPattern = ?Query:exec1(Ch2, reflib_dataflow:flow_back(), error),
            % ?d(Ch2VarPattern),
            % Ch2ReachList = ?Dataflow:reach_1st([Ch2VarPattern], [{back, true}]),
            % ?d(Ch2ReachList),
            % Ch2OutsideCons = lists:filter(fun(E) -> ?Expr:type(E) == cons end, Ch2ReachList),
            % ?d(Ch2OutsideCons),
            % case length(Ch2OutsideCons) of
            %     1 -> case check_children_number(Ch2OutsideCons) of
            %             % HIBA: --------------------- t1.erl ---------------------
            %             1 -> list_to_atom_sanitize(App, File, hd(Ch2OutsideCons));
            %             _ -> throw(?LocalError(replacable, []))
            %         end;
            %     _ -> throw(?LocalError(replacable, []))
            % end;
            %-----
            % Ch2VarPattern = ?Query:exec1(Ch2, reflib_dataflow:flow_back(), error),
            % ?d(Ch2VarPattern),
            % list_to_atom_sanitize(App, File, Ch2VarPattern);
            ok;
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
            case ?Expr:type(Ch1) of
                variable -> 
                    handle_variable_inside_application(App, File, Ch1);
                application -> 
                    case_of_application_arg(App, File, Ch1);
                parenthesis -> % Plusz zárójel, de csak 1 arg - annál több nem lehet, hiba
                    case check_children_number(Ch1) of
                        1 -> 
                            NextChild1 = ?Query:exec1(Ch1, ?Expr:child(1), error),
                            handle_variable_inside_application(App, File, NextChild1);
                        _ -> throw(?LocalError(replacable, [])) 
                    end;
                _ -> throw(?LocalError(replacable, []))
            end;
        2 -> % --------------------- t6.erl | t7.erl | t9.erl | t12.erl ---------------------
            Ch1 = ?Query:exec1(?Query:exec1(Arg, ?Expr:child(2), error), ?Expr:child(1), error),
            ?d(Ch1),
            Ch2 = ?Query:exec1(?Query:exec1(Arg, ?Expr:child(2), error), ?Expr:child(2), error),
            ?d(Ch2),
            ?d(?Expr:type(Ch1)),
            case ?Expr:type(Ch1) of
                variable ->
                    handle_variable_inside_application(App, File, Ch1);
                application -> 
                    case_of_application_arg(App, File, Ch1);
                parenthesis -> % Plusz zárójel, de csak 1 arg - annál több nem lehet, hiba
                    case check_children_number(Ch1) of
                        1 -> 
                            NextChild1 = ?Query:exec1(Ch1, ?Expr:child(1), error),
                            handle_variable_inside_application(App, File, NextChild1);
                        _ -> throw(?LocalError(replacable, [])) 
                    end;
                _ -> 
                    ?d(?Expr:type(Ch2)),
                    case ?Expr:type(Ch2) of
                        variable -> 
                            handle_variable_inside_application(App, File, Ch2);
                        application -> 
                            case_of_application_arg(App, File, Ch2);
                        parenthesis -> % Plusz zárójel, de csak 1 arg - annál több nem lehet, hiba
                            case check_children_number(Ch2) of
                                1 -> 
                                    NextChild2 = ?Query:exec1(Ch2, ?Expr:child(1), error),
                                    handle_variable_inside_application(App, File, NextChild2);
                                _ -> throw(?LocalError(replacable, [])) 
                            end;
                        _ -> throw(?LocalError(replacable, []))    
                    end
            end;
        _ -> throw(?LocalError(replacable, []))
    end
.

% t2.erl | t6.erl | t7.erl | t8.erl | t9.erl | t10.erl | t11.erl | t12.erl | t1.erl | t15.erl
%% If the untrusted argument is a variable inside an application
handle_variable_inside_application(App, File, Child) ->
    ?d("----- handle_variable_inside_application -----"),
    case ?Expr:role(Child) of
        expr ->
            Ch1Flow = ?Query:exec1(Child, reflib_dataflow:flow_back(), error),
            ?d(Ch1Flow),
            Ch1Deps =  reflib_dataflow:deps(Ch1Flow),   % UntrustedArg
            ?d(Ch1Deps),
            case length(Ch1Deps) of
                1 -> 
                    case ?Expr:role(hd(Ch1Deps)) of
                        expr -> 
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
                                                list_comp ->
                                                    list_to_atom_sanitize(ListCompWithOList, File, hd(Ch1Deps));
                                                _-> list_to_atom_sanitize(App, File, hd(Ch1Deps))
                                            end;
                                        _ -> list_to_atom_sanitize(App, File, hd(Ch1Deps))
                                    end;
                                _ -> list_to_atom_sanitize(App, File, hd(Ch1Deps))
                            end;
                        _ -> throw(?LocalError(replacable, []))
                    end;
                0 -> 
                    list_to_atom_sanitize(App, File, Child);
                _ -> throw(?LocalError(replacable, []))
            end;
        _ -> throw(?LocalError(replacable, []))
    end
.


% t3.erl | t5.erl | t13.erl | t14.erl 
%% If the untrusted argument is a variable
case_of_variable_arg(App, File, Arg) ->
    ?d("----- case_of_variable_arg -----"),
    VarFlow = ?Query:exec1(Arg, reflib_dataflow:flow_back(), error),
    ?d(VarFlow),
    VarDeps =  reflib_dataflow:deps(VarFlow),   % UntrustedArg
    ?d(VarDeps),
    case length(VarDeps) of
        1 -> 
            VarDepsWithOList = hd(VarDeps),
            ?d(?Expr:type(VarDepsWithOList)),
            ?d(?Expr:role(VarDepsWithOList)),
            case ?Expr:type(VarDepsWithOList) of
                variable ->
                    case ?Expr:role(VarDepsWithOList) of
                        expr -> % --------------------- t3.erl ---------------------
                                [{_, AppParent}] = ?Syn:parent(App),
                                case ?Clause:is_clause(AppParent) of
                                    true ->
                                        ListComp = ?Query:exec(AppParent, ?Clause:clauseof()),
                                        case length(ListComp) of
                                            1 ->
                                                ListCompWithOList = hd(ListComp),
                                                case ?Expr:type(ListCompWithOList) of
                                                    list_comp ->
                                                        list_to_atom_sanitize(ListCompWithOList, File, VarDepsWithOList);
                                                    _-> list_to_atom_sanitize(App, File, VarDepsWithOList)
                                                end;
                                            _ -> list_to_atom_sanitize(App, File, VarDepsWithOList)
                                        end;
                                    _ -> list_to_atom_sanitize(App, File, VarDepsWithOList)
                                end;
                        pattern -> % --------------------- t5.erl ---------------------
                            ExprClause = ?Query:exec1(VarDepsWithOList, ?Expr:clause(), error),
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
                                            FunName = ?Expr:value(?Query:exec1(?Query:exec1(ParentOfFunExpr, ?Expr:child(1), error), ?Expr:child(2), error)),
                                            ?d(FunName),
                                            Child2 = ?Query:exec1(ParentOfFunExpr, ?Expr:child(2), error),  % UntrustedArg
                                            ?d(Child2),
                                            MapAppParent = ?Query:exec1(?Query:exec1(Child2, ?Expr:parent(), error), ?Expr:parent(), error),
                                            ?d(MapAppParent),
                                            list_to_atom_sanitize(MapAppParent, File, Child2);
                                        _ -> throw(?LocalError(replacable, []))
                                    end;
                                _ -> throw(?LocalError(replacable, []))
                            end;
                        _ -> throw(?LocalError(replacable, []))
                    end
            end;
        0 -> % --------------------- t13.erl | t14.erl | t16.erl ---------------------
            case ?Expr:role(VarFlow) of
                pattern ->
                    ExprClause = ?Query:exec1(VarFlow, ?Expr:clause(), error),
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
                        fundef -> list_to_atom_sanitize(App, File, Arg); % t16.erl
                        _ -> throw(?LocalError(replacable, []))
                    end;
                _ -> throw(?LocalError(replacable, []))
            end;
        _ -> throw(?LocalError(replacable, []))
    end
.

%%% ============================================================================
%%% Sanitize

list_to_atom_sanitize(App, File, UntrustedArg)->
    ?d("--- SANITIZE ---"),
    [{_, AppParent}] = ?Syn:parent(App),
    [fun() ->
        %--- CRIT CHECK
        CaseSanitizeAtom = ?Syn:construct({atom, sanitize}),
        {_ , CaseSanitizeArg} = lists:keyfind(UntrustedArg, 1, ?Syn:copy(UntrustedArg)),
        CaseSanitizeArgList = ?Syn:create(#expr{type=arglist}, [{esub, CaseSanitizeArg}]),
        CaseSanitizeApp = ?Syn:create(#expr{type = application}, [{esub, [CaseSanitizeAtom]},{esub, CaseSanitizeArgList}]),

        %--- CASE - TRUE
        {_ , TrueListToAtomPart} = lists:keyfind(App, 1, ?Syn:copy(App)),
        TrueAtom = ?Syn:construct({atom, true}),
        TrueSanitizePattern = ?Syn:construct({pattern, [TrueAtom], [], [TrueListToAtomPart]}),

        %--- CASE - FALSE
        ThrowArg = ?Syn:construct({string, "Variable criteria not met"}),
        ThrowArgList = ?Syn:create(#expr{type=arglist}, [{esub,ThrowArg}]),
        ThrowAtom = ?Syn:construct({atom, throw}),
        ThrowApp = ?Syn:create(#expr{type = application}, [{esub, [ThrowAtom]},{esub, ThrowArgList}]),
        FalseAtom = ?Syn:construct({atom, false}),
        FalseSanitizePattern = ?Syn:construct({pattern, [FalseAtom], [], [ThrowApp]}),

        %--- CASE
        NewCase = ?Syn:construct({'case', CaseSanitizeApp, [TrueSanitizePattern, FalseSanitizePattern]}),

        %--- SANITIZE FUNCTION
        SanitizeFuncAtom = ?Syn:construct({atom, sanitize}),
        SanitizeFuncPattern = ?Syn:construct({var_pattern, "X"}),
        SanitizeFuncLeftAtom = ?Syn:construct({atom, length}),
        SanitizeFuncLeftVar = ?Syn:construct({var, "X"}),
        SanitizeFuncLeftArgList = ?Syn:create(#expr{type=arglist}, [{esub,SanitizeFuncLeftVar}]),
        SanitizeFuncLeft = ?Syn:create(#expr{type = application}, [{esub, [SanitizeFuncLeftAtom]},{esub, SanitizeFuncLeftArgList}]),
        SanitizeFuncRight = ?Syn:construct({integer, 50}),
        SanitizeFuncBody = ?Syn:construct({{infix_expr, '<'}, SanitizeFuncLeft, SanitizeFuncRight}),
        SanitizeFuncClause = ?Syn:construct({fun_clause, [SanitizeFuncAtom], [SanitizeFuncPattern], [], [SanitizeFuncBody]}),
        SanitizeFuncForm = ?Syn:construct({func, [SanitizeFuncClause]}),
        LastForm = ?Query:exec1(App, ?Query:seq([?Expr:clause(),
                                           ?Clause:funcl(),
                                           ?Clause:form()]),
                        error),
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
%%% Checks

check_children_number(Node) ->
    List = ?Query:exec(Node, ?Expr:children()),
    length(List)
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