-module(config).
-export([lookup/1, lookup/2, lookup_children/2]).

%% Exported: lookup

-spec lookup(config_serv:json_path()) -> any().

lookup([Name|_] = JsonPath) ->
    [{_, App}] = ets:lookup(config_serv, Name),
    {ok, JsonTerm} = application:get_env(App, Name),
    config_serv:json_lookup(JsonTerm, JsonPath).

-spec lookup(config_serv:json_path(), config_serv:json_value()) -> any().

lookup([Name|_] = JsonPath, DefaultValue) ->
    case ets:lookup(config_serv, Name) of
        [{_, App}] ->
            {ok, JsonTerm} = application:get_env(App, Name),
            case config_serv:json_lookup(JsonTerm, JsonPath) of
                not_found ->
                    DefaultValue;
                Value ->
                    Value
            end;
        [] ->
            DefaultValue
    end.

%% Exported: lookup_children

-spec lookup_children(config_serv:json_path(), any()) -> [any()].

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
