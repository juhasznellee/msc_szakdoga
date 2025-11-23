-module(t19).
-export([t/1]).

% FROM
t(Keys) ->
    [ begin
          {Key, RawValue} = my_module:extract(K),
          AtomKey = list_to_atom(Key),
          application:set_env(myapp, AtomKey, RawValue)
      end
      || #my_record{} = K <- Keys
    ].

% TO
% t(Keys) ->
%     case size_check(Keys
%             ) of
%         true ->
%             [begin
%                 {Key, RawValue} = my_module:extract(K),
%                 AtomKey = list_to_atom(Key),
%                 application:set_env(myapp, AtomKey, RawValue)
%             end
%             || #my_record{} = K<-Keys
%             ];
%         false -> throw("Variable criteria not met")
%     end.
% size_check(X) -> length(X) < 10000.