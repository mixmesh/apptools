-module(config).
-export([lookup/1, lookup/2, lookup/3, lookup_children/2]).

%% Exported: lookup

-spec lookup(config_serv:json_path()) -> config_serv:json_term().

lookup(JsonPath) ->
    config_serv:lookup(config_serv, JsonPath).

-spec lookup(atom() | config_serv:json_path(),
             config_serv:json_path() | config_serv:json_value()) ->
          config_serv:json_term().

lookup(App, JsonPath) when is_atom(App) ->
    config_serv:lookup_in_application(App, JsonPath);
lookup(JsonPath, DefaultValue) ->
    config_serv:lookup(config_serv, JsonPath, DefaultValue).

-spec lookup(atom(), config_serv:json_path(), config_serv:json_value()) ->
          config_serv:json_term().

lookup(App, JsonPath, DefaultValue) ->
    config_serv:lookup_in_application(App, JsonPath, DefaultValue).

%% Exported: lookup_children

-spec lookup_children(config_serv:json_path(), config_serv:json_value()) ->
          [config_serv:json_term()].

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
