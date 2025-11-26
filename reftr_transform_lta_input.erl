-module(reftr_transform_lta_input).
-vsn("$Rev$").

-export([prepare/1, error_text/2]).

-include("user.hrl").

%%% @private
prepare(Args) ->      %Args: module, range
    App = reftr_transform_common:get_application(Args),
    Function = ?Query:exec1(App, ?Expr:function(), error),
    Name = ?Fun:name(Function),
    case Name of
        list_to_atom ->
            Arg = ?Query:exec1(?Query:exec1(App, ?Expr:child(2), error), ?Expr:child(1), error),
            case ?Expr:type(Arg) of 
                infix_expr -> 
                   case_of_infix_expr_arg(App, Arg);
                application ->
                    case_of_application_arg(App, Arg);
                variable ->
                    case_of_variable_arg(App, Arg);
                string -> 
                    throw(?LocalError(no_multi_atom_gen, []));
                integer ->
                    throw(?LocalError(no_multi_atom_gen, []));
                _ -> throw(?LocalError(no_transformation, []))
            end;
        list_to_existing_atom -> ?LocalError(already_safe, [Name]);
        _ -> ?LocalError(bad_transformation_name, [Name])
    end   
.


%%% ============================================================================
%%% Untrusted argument sanitize

% t1.erl | t4.erl 
%% If the untrusted argument is an infix expression
case_of_infix_expr_arg(App, Arg) ->
    Ch1 = ?Query:exec1(Arg, ?Expr:child(1), error),
    Ch2 = ?Query:exec1(Arg, ?Expr:child(2), error),
    case lists:member(?Expr:type(Ch1), [application, cons, variable, infix_expr]) of
        true -> case_of_infix_expr_child(App, Ch1);
        false -> 
            case lists:member(?Expr:type(Ch2), [application, cons, variable, infix_expr]) of
                true -> case_of_infix_expr_child(App, Ch2);
                false -> throw(?LocalError(no_transformation, []))
            end
    end
.

case_of_infix_expr_child(App, Child) ->
    case ?Expr:type(Child) of
        application -> % t1.erl
            ChildAppArg = ?Query:exec1(?Query:exec1(Child, ?Expr:child(2), error), ?Expr:child(1), error),
            case ?Expr:type(ChildAppArg) of
                variable -> 
                    handle_variable_inside_application(App, ChildAppArg);
                _ -> throw(?LocalError(no_transformation, []))
            end;
        cons ->
            ListCompApp = get_list_comp_part(App),
            list_to_atom_sanitize(ListCompApp, Child);        
        variable -> % t4.erl
            case_of_variable_arg(App, Child);
        infix_expr ->
            case_of_infix_expr_arg(App, Child);
        _ -> ok
    end
.

%% If the untrusted argument is an application
case_of_application_arg(App, Arg) ->
    ChOfArg = ?Query:exec1(Arg, ?Expr:child(2), error),
    case check_children_number(ChOfArg) of    
        1 -> % --------------------- t2.erl | t8.erl | t10.erl | t11.erl ---------------------
            Ch1 = ?Query:exec1(ChOfArg, ?Expr:child(1), error),
            case_of_application_child(App, Ch1);
        2 -> % --------------------- t6.erl | t7.erl | t9.erl | t12.erl ---------------------
            Ch1 = ?Query:exec1(?Query:exec1(Arg, ?Expr:child(2), error), ?Expr:child(1), error),
            Ch2 = ?Query:exec1(?Query:exec1(Arg, ?Expr:child(2), error), ?Expr:child(2), error),
            case lists:member(?Expr:type(Ch1), [application, parenthesis, variable]) of
                true -> case_of_application_child(App, Ch1);
                false -> 
                    case lists:member(?Expr:type(Ch2), [application, parenthesis, variable]) of
                        true -> case_of_application_child(App, Ch2);
                        false -> throw(?LocalError(no_multi_atom_gen, []))
                    end
            end;
        _ -> throw(?LocalError(no_transformation, []))
    end
.

case_of_application_child(App, Child) ->
    case ?Expr:type(Child) of
        variable -> 
            handle_variable_inside_application(App, Child);
        application -> 
            case_of_application_arg(App, Child);
        parenthesis -> % Plusz zárójel, de csak 1 arg - annál több nem lehet, hiba
            case check_children_number(Child) of
                1 -> 
                    NextChild = ?Query:exec1(Child, ?Expr:child(1), error),
                    handle_variable_inside_application(App, NextChild);
                _ -> throw(?LocalError(no_transformation, [])) 
            end;
        _ -> throw(?LocalError(no_transformation, []))    
    end
.

%% If the untrusted argument is a variable inside an application
handle_variable_inside_application(App, Child) ->
    case ?Expr:role(Child) of
        expr ->
            {Length, DepsOrFlow} = get_flow_deps(Child),   % if Length = 0 -> Flow | if Lenght = 1 -> Deps
            case Length of
                1 ->
                    case ?Expr:role(DepsOrFlow) of
                        expr -> 
                            ListCompApp = get_list_comp_part(App),
                            list_to_atom_sanitize(ListCompApp, DepsOrFlow);
                        pattern -> find_var_called_by_func(DepsOrFlow, App);
                        _ -> throw(?LocalError(no_transformation, []))
                    end;
                0 -> % rekurzív külső hívás - t16.erl | lta_t16
                    find_var_called_by_func(DepsOrFlow, App);
                _ -> throw(?LocalError(no_transformation, []))
            end;
        _ -> throw(?LocalError(no_transformation, []))
    end
.

%% If the untrusted argument is a variable
case_of_variable_arg(App, Arg) ->
    {Length, DepsOrFlow} = get_flow_deps(Arg),  % if Length = 0 -> Flow | if Lenght = 1 -> Dep
    case Length of
        1 -> 
            case ?Expr:type(DepsOrFlow) of
                variable ->
                    case ?Expr:role(DepsOrFlow) of
                        expr -> % --------------------- t3.erl ---------------------
                                ListCompApp = get_list_comp_part(App),
                                list_to_atom_sanitize(ListCompApp, DepsOrFlow);
                        _ -> throw(?LocalError(no_transformation, []))
                    end;
                tuple ->
                    case ?Expr:role(DepsOrFlow) of
                        pattern -> % --------------------- t5.erl ---------------------
                            ExprClause = ?Query:exec1(DepsOrFlow, ?Expr:clause(), error),
                            case ?Clause:type(ExprClause) of
                                funexpr ->
                                    FunExpr = ?Query:exec1(ExprClause, ?Clause:clauseof(), error),
                                    case ?Expr:type(FunExpr) of
                                        fun_expr ->
                                            ParentOfFunExpr = ?Query:exec1(FunExpr, ?Expr:parent(), error),
                                            MapAppParent = ?Query:exec1(ParentOfFunExpr, ?Expr:parent(), error),
                                            FunName = ?Expr:value(?Query:exec1(?Query:exec1(MapAppParent, ?Expr:child(1), error), ?Expr:child(2), error)),
                                            case FunName of
                                                map ->
                                                    Child2 = ?Query:exec1(ParentOfFunExpr, ?Expr:child(2), error),
                                                    list_to_atom_sanitize(MapAppParent, Child2);
                                                _ -> throw(?LocalError(no_transformation, []))
                                            end;
                                        _ -> throw(?LocalError(no_transformation, []))
                                    end;
                                fundef -> throw(?LocalError(recursive, []));
                                block ->
                                    BlockExpr = ?Query:exec(ExprClause, ?Clause:clauseof()),
                                    case length(BlockExpr) of
                                        1 ->
                                            ListComp = ?Query:exec(hd(BlockExpr), [cons_e]),
                                            case length(ListComp) of
                                                1 ->
                                                    ListCompWithOList = hd(ListComp),
                                                    case ?Expr:type(ListCompWithOList) of
                                                        list_comp -> 
                                                            ArgApp = ?Query:exec1(DepsOrFlow, reflib_dataflow:flow_back(), error),
                                                            ArgPattern = ?Query:exec1(?Query:exec1(ArgApp, ?Expr:child(2), error), ?Expr:child(1), error),
                                                            BlockVar = ?Query:exec1(ArgPattern, reflib_dataflow:flow_back(), error),
                                                            list_to_atom_sanitize(ListCompWithOList, BlockVar);
                                                        _ -> throw(?LocalError(no_transformation, []))
                                                    end;
                                                _ -> throw(?LocalError(no_transformation, []))  
                                            end;
                                        _ -> throw(?LocalError(no_transformation, []))  
                                    end;
                                _ -> throw(?LocalError(no_transformation, []))
                            end;
                        _ -> throw(?LocalError(no_transformation, []))    
                    end;
                cons -> % --------------------- t4.erl ---------------------
                    ListCompApp = get_list_comp_part(App),
                    list_to_atom_sanitize(ListCompApp, DepsOrFlow)
            end;
        0 -> % --------------------- t13.erl | t14.erl | t16.erl ---------------------
            case ?Expr:role(DepsOrFlow) of
                pattern ->
                    ExprClause = ?Query:exec1(DepsOrFlow, ?Expr:clause(), error),
                    case ?Clause:type(ExprClause) of
                        funexpr ->
                            FunExpr = ?Query:exec1(ExprClause, ?Clause:clauseof(), error),
                            case ?Expr:type(FunExpr) of
                                fun_expr ->
                                    ParentOfFunExpr = ?Query:exec1(FunExpr, ?Expr:parent(), error),
                                    FoldlAppParent = ?Query:exec1(ParentOfFunExpr, ?Expr:parent(), error),
                                    FunName = ?Expr:value(?Query:exec1(?Query:exec1(FoldlAppParent, ?Expr:child(1), error), ?Expr:child(2), error)),
                                    case lists:member(FunName, [foldl, foldr]) of
                                        true ->
                                            Child3 = ?Query:exec1(ParentOfFunExpr, ?Expr:child(3), error),  % UntrustedArg
                                            case ?Expr:type(Child3) of
                                                variable ->
                                                    handle_variable_inside_application(FoldlAppParent, Child3);
                                                list_comp ->
                                                    [HexprClause, _ComprClause] = ?Query:exec(Child3,?Expr:clauses()),
                                                    Child3ArgApp = ?Query:exec1(HexprClause, ?Clause:body(), error),
                                                    case_of_application_arg(FoldlAppParent, Child3ArgApp);
                                                _ -> list_to_atom_sanitize(FoldlAppParent, Child3)
                                            end;
                                        false -> throw(?LocalError(no_transformation, []))
                                    end;
                                _ -> throw(?LocalError(no_transformation, []))
                            end;
                        fundef -> find_var_called_by_func(DepsOrFlow, App); %throw(?LocalError(recursive, []));  % t15.erl | lta_t15.erl
                        pattern ->
                            PatternFlow = ?Query:exec1(DepsOrFlow, reflib_dataflow:flow_back(), error),
                            case ?Expr:type(PatternFlow) of
                                application ->
                                    ChOfArg = ?Query:exec1(PatternFlow, ?Expr:child(2), error),
                                    case check_children_number(ChOfArg) of
                                        1 ->
                                            Ch1 = ?Query:exec1(?Query:exec1(PatternFlow, ?Expr:child(2), error), ?Expr:child(1), error),
                                            case lists:member(?Expr:type(Ch1), [atom, string, integer]) of
                                                true ->
                                                    throw(?LocalError(no_multi_atom_gen, []));
                                                false -> 
                                                    throw(?LocalError(no_transformation, []))
                                            end;
                                        2 ->
                                            Ch1 = ?Query:exec1(?Query:exec1(PatternFlow, ?Expr:child(2), error), ?Expr:child(1), error),
                                            Ch2 = ?Query:exec1(?Query:exec1(PatternFlow, ?Expr:child(2), error), ?Expr:child(2), error),
                                            case lists:member(?Expr:type(Ch1), [atom, string, integer]) of
                                                true -> throw(?LocalError(no_multi_atom_gen, []));
                                                false -> 
                                                    case lists:member(?Expr:type(Ch2), [atom, string, integer]) of
                                                        true -> throw(?LocalError(no_multi_atom_gen, []));
                                                        false -> throw(?LocalError(no_transformation, []))
                                                    end
                                            end;
                                        _ -> throw(?LocalError(no_transformation, []))
                                    end;
                                case_expr -> 
                                    ParentOfCaseExpr = ?Query:exec1(PatternFlow, ?Expr:parent(), error),
                                    list_to_atom_sanitize(ParentOfCaseExpr, PatternFlow);
                                atom -> throw(?LocalError(no_multi_atom_gen, []));
                                _ -> throw(?LocalError(no_transformation, []))
                            end;
                        _ -> throw(?LocalError(no_transformation, []))
                    end;
                _ -> throw(?LocalError(no_transformation, []))
            end;
        _ -> throw(?LocalError(no_transformation, []))
    end
.

%% If the untrusted argument comes from an outer function
find_var_called_by_func(Flow, App) ->
    NewFunFlow = ?Query:exec1(Flow, reflib_dataflow:flow_back(), error),
    NewFunVar =  ?Query:exec(NewFunFlow, [{call, back}]),
    case length(NewFunVar) of
        0 -> % t14.erl
            case ?Expr:type(NewFunFlow) of
                application ->
                    ListGen = ?Query:exec1(?Query:exec1(NewFunFlow, ?Expr:parent(), error), ?Expr:parent(), throw(?LocalError(no_transformation, []))),
                    case ?Expr:type(ListGen) of
                        list_gen -> 
                            NewListCompApp = get_list_comp_part(ListGen),
                            [_HexprClause, ComprClause] = ?Query:exec(ListGen,?Expr:clauses()),
                            Untrusted = ?Query:exec1(ComprClause, ?Clause:body(), error),
                            list_to_atom_sanitize(NewListCompApp, Untrusted);
                        _ -> throw(?LocalError(no_transformation, [])) %case_of_application_arg(App, NewFunFlow);
                    end;
                T when T =:= atom; T =:= string; T =:= integer -> throw(?LocalError(no_multi_atom_gen, []));
                _ -> throw(?LocalError(no_fun_call, []))
            end;
        1 -> % t16.erl | t17.erl
            NewFunVarWithOList = hd(NewFunVar),
            case ?Expr:type(NewFunVarWithOList) of
                variable ->
                    NewApp = ?Query:exec1(?Query:exec1(NewFunVarWithOList, ?Expr:parent(), error), ?Expr:parent(), error),
                    {NewLength, NewDepsOrFlow} = get_flow_deps(NewFunVarWithOList),
                    case NewLength of
                        1 ->
                            case ?Expr:role(NewDepsOrFlow) of
                                expr ->
                                    NewListCompApp = get_list_comp_part(NewApp),
                                    list_to_atom_sanitize(NewListCompApp, NewDepsOrFlow); %t16.erl
                                _ -> list_to_atom_sanitize(NewApp, NewDepsOrFlow)
                            end;
                        0 -> 
                            case is_equal(Flow,NewDepsOrFlow) of
                                true -> list_to_atom_sanitize(NewApp, hd(NewFunVar)); %t20.erl - rekurziv loop
                                false -> find_var_called_by_func(NewDepsOrFlow, App) %lta_t16.erl | t14.erl    
                            end
                    end;
                application ->
                    throw(?LocalError(no_transformation, []));
                _ -> throw(?LocalError(no_transformation, []))
            end;
        _ -> 
            NewFunVarApp = lists:filter(fun(E) -> ?Expr:type(E) == application end, NewFunVar),
            case length(NewFunVarApp) of
                1 -> % simple recursive
                    throw(?LocalError(recursive, []));
                    % FunCall = ?Query:exec1(?Query:exec1(hd(NewFunVarApp), ?Expr:parent(), error), ?Expr:parent(), error),
                    % list_to_atom_sanitize(FunCall, hd(NewFunVarApp)); % t17.erl
                _ -> % complex calls
                    NodesToModify = [ E || E <- lists:map(fun(E) -> get_nodes_to_modify(E) end, NewFunVar), E =/= false],
                    case need_to_transform(NodesToModify) of
                        [] ->
                            throw(?LocalError(no_multi_atom_gen, []));
                        ModList ->
                            % {Node, _} = lists:last(ModList),
                            % File = hd(?Syn:get_file(Node)),
                            % multiple_list_to_atom_sanitize_simple_file(ModList, File)
                            throw(?LocalError(multi_fun_call, []))
                            %handle_multiple_sanitize(ModList)
                    end
            end
    end
.


%%% ============================================================================
%%% Single transformation

list_to_atom_sanitize(App, UntrustedArg) ->
    File = hd(?Syn:get_file(UntrustedArg)),
    CheckFunExists = exists_check_function(File),
    [{_, AppParent}] = ?Syn:parent(App),
    [fun() ->
        {CaseSanitizeArg, NewCase} = create_new_case(App, UntrustedArg),
        SanitizeFuncForm = create_new_form(),

        FormIndex = get_form_index(File, App),

        ?Syn:replace(AppParent, {node, App}, [NewCase]),
        case CheckFunExists of
            true -> ok;
            false ->
                ?File:add_form(File, FormIndex + 1, SanitizeFuncForm)
        end,

        ?Transform:touch(CaseSanitizeArg)
    end]
.

create_new_case(App, UntrustedArg) ->
    %--- CRIT CHECK
    %{_ , CaseSanitizeArg} = lists:keyfind(UntrustedArg, 1, ?Syn:copy(UntrustedArg)), % ezzel is ugyan azt kapom. Benthagytam, mert a többi transzformációnál ezt használom.
    CaseSanitizeArg = proplists:get_value(UntrustedArg, ?Syn:copy(UntrustedArg)),
    CaseSanitizeApp = ?Syn:construct({app, {atom, size_check}, [CaseSanitizeArg]}),

    %--- CASE - TRUE
    %{_ , FunctionPart} = lists:keyfind(App, 1, ?Syn:copy(App)), % ezzel is ugyan azt kapom. Benthagytam, mert a többi transzformációnál ezt használom.
    FunctionPart = proplists:get_value(App, ?Syn:copy(App)),
    TrueSanitizePattern = ?Syn:construct({pattern, [{atom, true}], [], [FunctionPart]}),

    %--- CASE - FALSE
    ThrowApp = ?Syn:construct({app, {atom, throw}, [{string, "Variable criteria not met"}]}),
    FalseSanitizePattern = ?Syn:construct({pattern, [{atom, false}], [], [ThrowApp]}),
    %--- CASE
    NewCase = ?Syn:construct({'case', CaseSanitizeApp, [TrueSanitizePattern, FalseSanitizePattern]}),
    {CaseSanitizeArg, NewCase}
.

create_new_form() ->
    %--- SANITIZE FUNCTION
    SanitizeFuncLeft = ?Syn:construct({app, {atom, length}, [{var, "X"}]}),
    SanitizeFuncRight = ?Syn:construct({integer, 10000}),
    SanitizeFuncClause = ?Syn:construct({fun_clause, 
                            [{atom, size_check}], 
                            [{var_pattern, "X"}], 
                            [], 
                            [{{infix_expr, '<'}, SanitizeFuncLeft, SanitizeFuncRight}]}),
    SanitizeFuncForm = ?Syn:construct({func, [SanitizeFuncClause]}),
    SanitizeFuncForm
.


%%% ============================================================================
%%% Multiple transformation

% handle_multiple_sanitize(ListOfMod) -> % Case of multiple file insert - This version is not working
%     FileList = lists:map(fun({App, _}) -> hd(?Syn:get_file(App)) end, ListOfMod),
%     UniqFileList = lists:uniq(FileList),
%     ExistList = lists:map(fun(File) -> exists_check_function(File) end, UniqFileList),
%     FormIndexList = lists:map(
%             fun({{App, _}, File}) ->
%                 get_form_index(File, App)
%             end,
%             lists:zip(ListOfMod, FileList)
%         ),
%     UniqFormList = lists:uniq(FormIndexList),
%     AppParentList = lists:map(fun({App, _}) -> get_app_parent_for_multiple_insert(App) end, ListOfMod),
%     multiple_list_to_atom_sanitize(UniqFileList, ExistList, UniqFormList, AppParentList, ListOfMod)
% .

% multiple_list_to_atom_sanitize(UniqFileList, ExistList, UniqFormList, AppParentList, ListOfMod) ->
%     NewConstructionList = lists:map(fun({App, Arg}) -> create_new_case(App, Arg) end, ListOfMod),

%     lists:map(
%         fun({{AppParent, App}, {_, NewCase}}) ->
%             ?Syn:replace(AppParent, {node, App}, [NewCase])
%         end,
%         lists:zip(AppParentList, NewConstructionList)
%     ),

%     SanitizeFuncForm = create_new_form(),

%     lists:map(
%       fun({File, Exists, Form}) ->
%             insert_forms(File, Exists, Form, SanitizeFuncForm)
%       end,
%       lists:zip3(UniqFileList, ExistList, UniqFormList)
%     ),

%     {Arg, _} = lists:last(NewConstructionList),
%     ?Transform:touch(Arg)
% .

% insert_forms(File, Exists, Index, Form) ->
%     case Exists of
%         true -> ok;
%         false ->
%             ?File:add_form(File, Index + 1, Form)
%     end
% .

% multiple_list_to_atom_sanitize_simple_file(ListOfMod, File) -> % Case of single file insert - This version is working, but PP inserts the code without spaces
%     CheckFunExists = exists_check_function(File),
%     FormIndexList = lists:map(fun({App, _}) -> get_form_index(File, App) end, ListOfMod),
%     UniqFormList = lists:uniq(FormIndexList),
%     AppParentList = lists:map(fun({App, _}) -> get_app_parent_for_multiple_insert(App) end, ListOfMod),
%     [fun() ->
%         NewConstructionList = lists:map(fun({App, Arg}) -> create_new_case(App, Arg) end, ListOfMod),
%         SanitizeFuncForm = create_new_form(),

%         lists:map(
%             fun({{AppParent, App}, {_, NewCase}}) ->
%                 ?Syn:replace(AppParent, {node, App}, [NewCase])
%             end,
%             lists:zip(AppParentList, NewConstructionList)
%         ),

%         case CheckFunExists of
%             true -> ok;
%             false ->
%                 lists:map(fun(E) -> ?File:add_form(File, E + 1, SanitizeFuncForm) end, UniqFormList)
%         end,
%         {Parent, _} = lists:last(AppParentList),
%         ?Transform:touch(Parent)
%     end]
% .

% get_app_parent_for_multiple_insert(App) ->
%     [{_, AppParent}] = ?Syn:parent(App),
%     {AppParent, App}
% .


%%% ===========================================================================
%%% Helper functions

is_equal(Node1, Node2) ->
    Node1 =:= Node2.

need_to_transform(ListOfNodes) ->
    lists:filter(fun({_, Arg}) -> not lists:member(?Expr:type(Arg), [atom, string, integer, tuple]) end, ListOfNodes).

get_nodes_to_modify(Node) ->
    case ?Expr:type(Node) of
        variable ->
            NewApp = ?Query:exec1(?Query:exec1(Node, ?Expr:parent(), error), ?Expr:parent(), error),
            {Length, DepsOrFlow} = get_flow_deps(Node),
            case Length of
                1 ->
                    case ?Expr:role(DepsOrFlow) of
                        expr ->
                            ListCompApp = get_list_comp_part(NewApp),
                            {ListCompApp, DepsOrFlow};
                        _ -> {NewApp, DepsOrFlow}
                    end;
                0 -> false
            end;
        application ->
            FunCall = ?Query:exec1(?Query:exec1(Node, ?Expr:parent(), error), ?Expr:parent(), error),
            {FunCall, Node};
        _ -> false
    end
.

get_flow_deps(Arg) ->
    Flow = ?Query:exec1(Arg, reflib_dataflow:flow_back(), error),
    DepsList = reflib_dataflow:deps(Flow),
    case length(DepsList) of
        1 -> {1, hd(DepsList)};
        0 -> {0, Flow}
    end
.

get_list_comp_part(App) ->
    [{_, AppParent}] = ?Syn:parent(App),
    case ?Clause:is_clause(AppParent) of
        true ->
            ListComp = ?Query:exec(AppParent, ?Clause:clauseof()),
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

check_children_number(Node) ->
    List = ?Query:exec(Node, ?Expr:children()),
    length(List)
.

get_form_index(File, App) ->
    Form = ?Query:exec1(App, ?Query:seq([?Expr:clause(), ?Clause:funcl(), ?Clause:form()]), error),
    FormFile = ?Query:exec(Form, ?Form:file()),
    case File of
        FormFile -> ?Syn:index(File, form, Form);
        _        -> length(?Query:exec(File, ?File:forms()))
    end
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


%%% ============================================================================
%%% Error messages

error_text(bad_transformation_name, [Name]) ->
    ?MISC:format("There is no given transformation for ~p function", [Name]);
error_text(no_fun_call, []) ->
    ?MISC:format("Function call not found.", []);
error_text(multi_fun_call, []) ->
    ?MISC:format("Multiple functions call the function containing list_to_atom, there is no transformation for this.", []);
error_text(no_multi_atom_gen, []) ->
    ?MISC:format("No transformation is needed because there is no multiple atom generation.", []);
error_text(already_safe, [Name]) ->
    ?MISC:format("The ~p function is already safe.", [Name]);
error_text(recursive, []) ->
    ?MISC:format("This is a recursive function, there is no transformation for that.", []);
error_text(no_transformation, []) ->
    ?MISC:format("No transformation for this case.", []).