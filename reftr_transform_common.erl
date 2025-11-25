-module(reftr_transform_common).
-vsn("$Rev$").

-export([get_application/1]).

-include("user.hrl").

%%% @private
get_application(Args) ->      %Args: module, range
    App = ?Args:expr_range(Args),
    Type = ?Expr:type(hd(App)),
    case Type of
        application -> App;
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
            AtomParent = ?Query:exec1(App, ?Expr:parent(), error),
            case ?Expr:type(AtomParent) of
                application -> AtomParent;
                infix_expr ->
                    InfixParent = ?Query:exec1(AtomParent, ?Expr:parent(), error),
                    case ?Expr:type(InfixParent) of
                        application -> InfixParent;
                        _ -> throw(?RefErr0r(bad_range)) 
                    end;
                _ -> throw(?RefErr0r(bad_range))
            end;
        infix_expr ->
            InfixParent = ?Query:exec1(App, ?Expr:parent(), error),
            case ?Expr:type(InfixParent) of
                application -> InfixParent;
                _ -> throw(?RefErr0r(bad_range)) 
            end;
        _ -> throw(?RefErr0r(bad_range))
    end
.