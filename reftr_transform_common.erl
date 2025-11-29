-module(reftr_transform_common).
-vsn("$Rev$").

-export([get_application_node_from_arg/1, error_text/2]).

-include("user.hrl").

%%% @private
get_application_node_from_arg(Args) ->      %Args: module, range
    App = ?Args:expr_range(Args),
    Type = ?Expr:type(hd(App)),
    case Type of
        application -> hd(App);
        list_comp -> 
            [HexprClause, _ComprClause] = ?Query:exec(App,?Expr:clauses()),
            ClauseParent = ?Query:exec1(HexprClause, ?Clause:body(), error),
            case ?Expr:type(ClauseParent) of
                application -> ClauseParent;
                infix_expr ->
                    InfixParent = ?Query:exec1(ClauseParent, ?Expr:parent(), error),
                    case ?Expr:type(InfixParent) of
                        application -> InfixParent;
                        _ -> throw(?RefErr0r(bad_range)) 
                    end;
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        atom -> 
            AtomParent = ?Query:exec1(App, ?Expr:parent(), ?LocalError(no_transformation, [])),
            case ?Expr:type(AtomParent) of
                application -> AtomParent;
                infix_expr ->
                    InfixParent = ?Query:exec1(AtomParent, ?Expr:parent(), error),
                    case ?Expr:type(InfixParent) of
                        application -> InfixParent;
                        _ -> throw(?RefErr0r(bad_range)) 
                    end;
                implicit_fun ->
                    throw(?LocalError(no_transformation, []));
                _ -> throw(?RefErr0r(bad_range))
            end;
        infix_expr ->
            InfixParent = ?Query:exec1(App, ?Expr:parent(), error),
            case ?Expr:type(InfixParent) of
                application -> InfixParent;
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        cons ->
            ConsApp = ?Query:exec1(App, [{cons_e, back}], error),
            case ?Expr:type(ConsApp) of
                application -> ConsApp;
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        _ -> throw(?RefErr0r(bad_range))
    end
.


%%% ============================================================================
%%% Error messages

error_text(no_transformation, []) ->
    ?MISC:format("No transformation for this case.", []).