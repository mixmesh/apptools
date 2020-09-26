-module(config).
-export([lookup/1, lookup/2, lookup_children/2]).

%% Exported: lookup

lookup(JsonPath) ->
    config_serv:lookup(obscrete_config_serv, JsonPath).

lookup(JsonPath, DefaultValue) ->
    config_serv:lookup(obscrete_config_serv, JsonPath, DefaultValue).

%% Exported: lookup_children

lookup_children([], _KeyValueList) ->
    [];
lookup_children(_Keys, []) ->
    [];
lookup_children(Keys, [{Key, Value}|Rest]) ->
    case lists:member(Key, Keys)  of
        true ->
            [Value|lookup_children(lists:delete(Key, Keys), Rest)];
        false ->
            lookup_children(Keys, Rest)
    end.
