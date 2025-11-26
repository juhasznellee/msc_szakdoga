-module(t20).
-export([get_module_proc/2]).

% FROM
get_module_proc(Host, Base) when is_binary(Host) ->
    get_module_proc(binary_to_list(Host), Base);
get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).

% TO
% get_module_proc(Host, Base) when is_binary(Host) ->
%     case size_check(Base) of
%         true -> get_module_proc(binary_to_list(Host), Base);
%         false -> throw("Variable criteria not met")
%     end;
% get_module_proc(Host, Base) ->
%     list_to_atom(atom_to_list(Base) ++ "_" ++ Host).

% size_check(X) -> length(X) < 10000.