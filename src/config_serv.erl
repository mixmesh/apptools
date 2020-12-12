-module(config_serv).
-export([start_link/4]).
-export([subscribe/0, subscribe/1, export_config_file/0]).
-export([json_lookup/2, json_path_to_string/1]).
-export([tcp_send/3]).
-export([atomify/1, lookup_schema/2, convert/4]).
-export([unconvert_values/2, unconvert_value/2]).
-export([format_error/1]).
-export([message_handler/1]).

-export_type([type_name/0, json_path/0, json_value/0, error_reason/0]).

-include_lib("kernel/include/file.hrl").
-include("../include/config_schema.hrl").
-include("../include/shorthand.hrl").
-include("../include/serv.hrl").

-record(state,
        {parent :: pid(),
         tcp_serv :: pid(),
         config_filename :: file:filename(),
         app_schemas :: [{atom(), schema()}],
         subscribers = [] :: [pid()]}).

-type type_name() ::
        bool |
        {integer, integer(), integer() | unbounded} |
        {float, float(), float() | unbounded} |
	ip_address |
	ip_address_port |
        ip4_address |
        ip4_address_port |
        ip6_address |
        ip6_address_port |
        hostname_port |       
        interface_port |
        base64 |
        readable_file |
        writable_file |
        readable_directory |
        writable_directory |
        atom |
        string |
        path.
-type ip_address_port() :: {inet:ip_address(), inet:port_number()}.
-type ip4_address_port() :: {inet:ip4_address(), inet:port_number()}.
-type ip6_address_port() :: {inet:ip6_address(), inet:port_number()}.
-type hostname_port() :: {string(), inet:port_number()}.
-type interface_port() :: {string(), inet:port_number()}.
-type json_value() ::
        boolean() |
        integer() |
        float() |
        inet:ip_address() |
        ip_address_port() |
        ip4_address_port() |
        ip6_address_port() |
        hostname_port() |
        interface_port() |
        binary() |
        atom() |
        null.
-type schema() :: [{atom(), #json_type{}}] |
                  [{atom(), schema()}] |
                  [schema()].
-type json_name() :: atom() | {atom(), json_value()}.
-type json_path() :: [json_name()].
-type error_reason() ::
        {config, config_error_reason()} |
        {posix, inet:posix()}.
-type config_error_reason() ::
        {bad_json, term()} |
        {file_error, file:filename(), file:posix()} |
        {file_error, file:filename(), file:posix(), json_path()} |
        {missing, json_name()} |
        {unexpected, json_name()} |
        {expected, json_path(), json_path()} |
        {not_bool, json_value(), json_path()} |
        {integer_out_of_range, json_value(), integer(), integer(),
         json_path()} |
        {not_integer, json_value(), json_path()} |
        {float_out_of_range, json_value(), float(), float(),
         json_path()} |
        {not_float, json_value(), json_path()} |
        {not_ip_address, json_value(), json_path()} |
	{not_ip_address_port, json_value(), json_path()} |
        {not_ip4_address, json_value(), json_path()} |
        {not_ip4_address_port, json_value(), json_path()} |
        {not_ip6_address, json_value(), json_path()} |
        {not_ip6_address_port, json_value(), json_path()} |
        {not_hostname_port, json_value(), json_path()} |
        {not_interface_port, json_value(), json_path()} |
        {not_base64, json_value(), json_path()} |
        {not_readable_file, string(), json_path()} |
        {not_writable_file, string(), json_path()} |
        {not_readable_directory, string(), json_path()} |
        {not_writable_directory, string(), json_path()} |
        {not_atom, json_value(), json_path()} |
        {not_string, json_value(), json_path()} |
        {invalid_value, json_value(), json_path()} |
        {invalid_transform_value, json_path(), string()}.

-type read_config() :: fun(() -> ip4_address_port()).

%% Exported: start_link

-spec start_link(ConfigFilename :: file:filename(),
                 AppSchemas :: [{atom(), schema()}],
                 ReadConfig :: read_config(),
                 Handler :: fun((gen_tcp:socket()) -> ok)) ->
          serv:spawn_server_result() |
          {config, config_error_reason()}.

start_link(ConfigFilename, AppSchemas, ReadConfig, Handler) ->
    ?spawn_server_opts(
       fun(Parent) ->
               init(Parent, ConfigFilename, AppSchemas, ReadConfig, Handler)
       end,
       fun ?MODULE:message_handler/1,
       #serv_options{name = ?MODULE}).

%% Exported: subscribe

-spec subscribe(pid()) -> ok.

subscribe() ->
    serv:cast(?MODULE, {subscribe, self()}).

subscribe(Pid) ->
    serv:cast(?MODULE, {subscribe, Pid}).

%% Exported: export_config_file

-spec export_config_file() -> ok.

export_config_file() ->
    serv:call(?MODULE, export_config_file).

%% Exported: json_lookup

json_lookup(JsonTerm, []) ->
    JsonTerm;
json_lookup(JsonTerm, [Name]) when is_atom(Name), is_list(JsonTerm) ->
    case lists:keysearch(Name, 1, JsonTerm) of
        {value, {Name, NestedJsonTermOrValue}} ->
            NestedJsonTermOrValue;
        false ->
            not_found
    end;
%% Note: For now a key can only be the last item in the path. This
%% restriction will be lifted when needed.
json_lookup(JsonTerm, [{KeyName, Value}]) when is_list(JsonTerm) ->
    json_lookup_instance(JsonTerm, {KeyName, Value});
json_lookup(JsonTerm, [Name|Rest]) when is_atom(Name), is_list(JsonTerm) ->
    case lists:keysearch(Name, 1, JsonTerm) of
        {value, {Name, NestedJsonTerm}} ->
            json_lookup(NestedJsonTerm, Rest);
        false ->
            not_found
    end.

json_lookup_instance([], {_KeyName, _Value}) ->
    not_found;
json_lookup_instance([JsonTermInstance|Rest], {KeyName, Value}) ->
    case lists:member({KeyName, Value}, JsonTermInstance)of
        true ->
            JsonTermInstance;
        false ->
            json_lookup_instance(Rest, {KeyName, Value})
    end.

%% Exported: json_path_to_string

json_path_to_string([Name|Rest]) ->
    json_path_to_string(Rest, [?a2l(Name)]).

json_path_to_string([], Acc) ->
    Acc;
json_path_to_string([Name|Rest], Acc) ->
    json_path_to_string(Rest, [?a2l(Name), $/|Acc]).

%% Exported: tcp_send

-spec tcp_send(inet:ip_address(), inet:port_number(), Message :: binary()) ->
          ok | {error, {posix, inet:posix()}}.

tcp_send(Address, Port, Message) ->
    case gen_tcp:connect(Address, Port,
                         [{packet, 2}, {nodelay, true}, binary]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, Message),
            gen_tcp:close(Socket);
        {error, Reason} ->
            {error, {posix, Reason}}
    end.

%% Exported: atomify

atomify([]) ->
    [];
atomify([{Binary, JsonTerm}|Rest]) when is_list(JsonTerm), is_list(Rest) ->
    [{?b2a(Binary), atomify(JsonTerm)}|atomify(Rest)];
atomify([{Binary, JsonValue}|Rest]) when is_list(Rest) ->
    [{?b2a(Binary), JsonValue}|atomify(Rest)];
atomify([JsonTerm|Rest]) when is_list(JsonTerm), is_list(Rest) ->
    [atomify(JsonTerm)|atomify(Rest)];
atomify([JsonValue|Rest]) when is_list(Rest) ->
    [JsonValue|atomify(Rest)].

load_json_term(ConfigDir, AppSchemas, JsonTerm) ->
    {App, FirstNameInJsonPath, Schema, RemainingAppSchemas} =
        lookup_schema(AppSchemas, JsonTerm),
    case convert(ConfigDir, Schema, JsonTerm, false) of
        {ConvertedJsonTerm, []} when RemainingAppSchemas == [] ->
            ok = application:set_env(
                   App, FirstNameInJsonPath, ConvertedJsonTerm,
                   [{persistent, true}]),
            true = ets:insert(?MODULE, {FirstNameInJsonPath, App});
        {_ConvertedJsonTerm, []} ->
            [{_App, [{Name, _JsonTerm}|_]}|_] = RemainingAppSchemas,
            throw({missing, Name});
        {ConvertedJsonTerm, RemainingJsonTerm} ->
            ok = application:set_env(
                   App, FirstNameInJsonPath, ConvertedJsonTerm,
                   [{persistent, true}]),
            true = ets:insert(?MODULE, {FirstNameInJsonPath, App}),
            load_json_term(ConfigDir, RemainingAppSchemas,
                           RemainingJsonTerm)
    end.

%% Exported: lookup_schema

lookup_schema(AppSchemas, JsonTerm) ->
    lookup_schema(AppSchemas, JsonTerm, []).

lookup_schema([], [{Name, _JsonValue}|_], _MismatchedAppSchemas) ->
    throw({unexpected, Name});
lookup_schema([{App, [{Name, _JsonType}|_] = Schema}|Rest],
              [{Name, _JsonValue}|_], MismatchedAppSchemas) ->
    {App, Name, Schema, MismatchedAppSchemas ++ Rest};
lookup_schema([{_App, [{_Name, _JsonType}|_]} = AppSchema|Rest],
              [{_AnotherName, _JsonValue}|_] = JsonTerm,
              MismatchedAppSchemas) ->
    lookup_schema(Rest, JsonTerm, [AppSchema|MismatchedAppSchemas]).

%% Exported: convert

convert(ConfigDir, Schema, JsonTerm, Lazy) ->
    convert(ConfigDir, Schema, JsonTerm, Lazy, [], []).

convert(_ConfigDir, _Schema, [], true, _JsonPath,
        ConvertedJsonTerm) ->
    {lists:reverse(ConvertedJsonTerm), []};
convert(_ConfigDir, [], RemainingJsonTerm, _Lazy, _JsonPath,
        ConvertedJsonTerm) ->
    {lists:reverse(ConvertedJsonTerm), RemainingJsonTerm};
%% Single value
convert(ConfigDir,
        [{Name, JsonType}|SchemaRest],
        [{Name, JsonValue}|JsonTermRest], Lazy, JsonPath,
        ConvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    ConvertedValue =
        convert_value(ConfigDir, JsonType, JsonValue, [Name|JsonPath]),
    convert(ConfigDir, SchemaRest, JsonTermRest, Lazy, JsonPath,
            [{Name, ConvertedValue}|ConvertedJsonTerm]);
convert(_ConfigDir,
        [{Name, JsonType}|_SchemaRest],
        [{AnotherName, _JsonValue}|_JsonTermRest], false, JsonPath,
        _ConvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
convert(ConfigDir,
        [{_Name, JsonType}|SchemaRest],
        [{AnotherName, JsonValue}|JsonTermRest], true, JsonPath,
        ConvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    convert(ConfigDir,
            SchemaRest,
            [{AnotherName, JsonValue}|JsonTermRest], true, JsonPath,
            ConvertedJsonTerm);
%% Array of single values
convert(ConfigDir,
        [{Name, [JsonType]}|SchemaRest],
        [{Name, JsonValues}|JsonTermRest], Lazy, JsonPath,
        ConvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    ConvertedValues =
        convert_values(ConfigDir, JsonType, JsonValues, [Name|JsonPath]),
    convert(ConfigDir, SchemaRest, JsonTermRest, Lazy, JsonPath,
            [{Name, ConvertedValues}|ConvertedJsonTerm]);
convert(_ConfigDir,
        [{Name, [JsonType]}|_SchemaRest],
        [{AnotherName, _JsonValue}|_JsonTermRest], false, JsonPath,
        _ConvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
convert(ConfigDir,
        [{_Name, [JsonType]}|SchemaRest],
        [{AnotherName, JsonValue}|JsonTermRest], true, JsonPath,
        ConvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    convert(ConfigDir,
            SchemaRest,
            [{AnotherName, JsonValue}|JsonTermRest], true, JsonPath,
            ConvertedJsonTerm);
%% Object
convert(ConfigDir,
        [{Name, NestedSchema}|SchemaRest],
        [{Name, NestedJsonTerm}|JsonTermRest], Lazy, JsonPath,
        ConvertedJsonTerm) ->
    case Lazy of
        false ->
            {ConvertedNestedJsonTerm, []} =
                convert(ConfigDir, NestedSchema, NestedJsonTerm, Lazy, [Name|JsonPath], []);
        true ->
            {ConvertedNestedJsonTerm, RemainingJsonTerm} =
                convert(ConfigDir, NestedSchema, NestedJsonTerm, Lazy, [Name|JsonPath], []),
            case RemainingJsonTerm of
                [] ->
                    ok;
                [{AnotherName, _}|_] ->
                    throw({unexpected, AnotherName})
            end
    end,
    convert(ConfigDir, SchemaRest, JsonTermRest, Lazy, JsonPath,
            [{Name, ConvertedNestedJsonTerm}|ConvertedJsonTerm]);
%% Mismatch
convert(_ConfigDir,
        [{Name, _NestedSchema}|_SchemaRest],
        [{AnotherName, _NestedJsonTerm}|_JsonTermRest], false, JsonPath,
        _ConvertedJsonTerm) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
convert(ConfigDir,
        [{_Name, _NestedSchema}|SchemaRest],
        [{AnotherName, NestedJsonTerm}|JsonTermRest], true, JsonPath,
        ConvertedJsonTerm) ->
    convert(ConfigDir,
            SchemaRest,
            [{AnotherName, NestedJsonTerm}|JsonTermRest], true, JsonPath,
            ConvertedJsonTerm);
%% List
convert(ConfigDir,
        [Schema|SchemaRest],
        [JsonTerm|JsonTermRest], Lazy, JsonPath,
        ConvertedJsonTerm)
  when is_list(Schema), is_list(JsonTerm) ->
    {ConvertedFirstJsonTerm, []} =
        convert(ConfigDir, Schema, JsonTerm, Lazy, JsonPath, []),
    convert(ConfigDir, [Schema|SchemaRest], JsonTermRest, Lazy, JsonPath,
            ConvertedFirstJsonTerm ++ ConvertedJsonTerm).

%% bool
convert_value(_ConfigDir, #json_type{name = bool, transform = Transform}, Value,
              JsonPath)
  when is_boolean(Value) ->
    transform_value(Transform, Value, JsonPath);
convert_value(_ConfigDir, #json_type{name = bool}, Value, JsonPath) ->
    throw({not_bool, Value, JsonPath});
%% integer
convert_value(_ConfigDir, #json_type{name = {integer, From, unbounded},
                                     transform = Transform},
              Value, JsonPath)
  when is_integer(Value) andalso Value >= From andalso Value >= From ->
    transform_value(Transform, Value, JsonPath);
convert_value(_ConfigDir, #json_type{name = {integer, From, To},
                                     transform = Transform}, Value,
              JsonPath)
  when is_integer(Value) andalso Value >= From andalso Value =< To ->
    transform_value(Transform, Value, JsonPath);
convert_value(_ConfigDir, #json_type{name = {integer, From, To}}, Value,
              JsonPath)
  when is_integer(Value) ->
    throw({integer_out_of_range, Value, From, To, JsonPath});
convert_value(_ConfigDir, #json_type{name = {integer, _From, _To}}, Value,
              JsonPath) ->
    throw({not_integer, Value, JsonPath});
%% float
convert_value(_ConfigDir, #json_type{name = {float, From, unbounded},
                                     transform = Transform},
              Value, JsonPath)
  when is_number(Value) andalso Value >= From andalso Value >= From ->
    transform_value(Transform, Value * 1.0, JsonPath);
convert_value(_ConfigDir, #json_type{name = {float, From, To},
                                     transform = Transform}, Value,
              JsonPath)
  when is_number(Value) andalso Value >= From andalso Value =< To ->
    transform_value(Transform, Value * 1.0, JsonPath);
convert_value(_ConfigDir, #json_type{name = {float, From, To}}, Value,
              JsonPath)
  when is_float(Value) ->
    throw({float_out_of_range, Value, From, To, JsonPath});
convert_value(_ConfigDir, #json_type{name = {float, _From, _To}}, Value,
              JsonPath) ->
    throw({not_float, Value, JsonPath});
%% ip_address
convert_value(_ConfigDir, #json_type{name = ip_address,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case inet:parse_address(?b2l(Value)) of
        {ok, IpAddress} ->
            transform_value(Transform, IpAddress, JsonPath);
        {error, einval} ->
            throw({not_ip_address, Value, JsonPath})
    end;
%% ip_address_port 1.2.3.4:80 or [10:11:12:13::abcd]:80
convert_value(_ConfigDir, #json_type{name = ip_address_port,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case string:rchr(?b2l(Value), $:) of
	0 ->
	    throw({not_ip_address_port, Value, JsonPath});
	Pos ->
	    %% [1:2:3:4]:80  Pos-1 = [1:2:3:4] Pos-3 = 1:2:3:4
	    Size6 = Pos-3,
	    case split_binary(Value, Pos-1) of
		{<<$[,IpValue:Size6/binary,$]>>,<<$:,PortValue/binary>>} ->
		    case inet:parse_ipv6_address(?b2l(IpValue)) of
			{ok, Ipv6Address} ->
			    case catch ?b2i(PortValue) of
				Port when is_integer(Port) ->
				    transform_value(Transform, {Ipv6Address, Port},
                                                    JsonPath);
				_ ->
				    throw({not_ip_address_port, Value, JsonPath})
			    end;
			{error, einval} ->
			    throw({not_ip_address_port, Value, JsonPath})
		    end;
		{IpValue,<<$:,PortValue/binary>>} ->
		    case inet:parse_ipv4_address(?b2l(IpValue)) of
			{ok, Ip4Address} ->
			    case catch ?b2i(PortValue) of
				Port when is_integer(Port) ->
				    transform_value(Transform, {Ip4Address, Port},
                                                    JsonPath);
				_ ->
				    throw({not_ip_address_port, Value, JsonPath})
			    end;
			{error, einval} ->
			    throw({not_ip_address, Value, JsonPath})
		    end;
		_ ->
		    throw({not_ip_address_port, Value, JsonPath})
	    end
    end;
convert_value(_ConfigDir, #json_type{name = ip_address_port}, Value,
              JsonPath) ->
    throw({not_ip_address_port, Value, JsonPath});
%% ip4_address
convert_value(_ConfigDir, #json_type{name = ip4_address,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case inet:parse_ipv4_address(?b2l(Value)) of
        {ok, Ip4Address} ->
            transform_value(Transform, Ip4Address, JsonPath);
        {error, einval} ->
            throw({not_ip4_address, Value, JsonPath})
    end;
%% ip4_address_port
convert_value(_ConfigDir, #json_type{name = ip4_address_port,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
        [Ip4AddressString, PortString] ->
            case inet:parse_ipv4_address(Ip4AddressString) of
                {ok, Ip4Address} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            transform_value(Transform, {Ip4Address, Port},
                                            JsonPath);
                        _ ->
                            throw({not_ip4_address_port, Value, JsonPath})
                    end;
                {error, einval} ->
                    throw({not_ip4_address_port, Value, JsonPath})
            end;
        _ ->
            throw({not_ip4_address_port, Value, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = ip4_address_port}, Value,
              JsonPath) ->
    throw({not_ip4_address_port, Value, JsonPath});
%% ip6_address
convert_value(_ConfigDir, #json_type{name = ip6_address,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case inet:parse_ipv6_address(?b2l(Value)) of
        {ok, Ip6Address} ->
            transform_value(Transform, Ip6Address, JsonPath);
        {error, einval} ->
            throw({not_ip6_address, Value, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = ip6_address}, Value, JsonPath) ->
    throw({not_ip6_address, Value, JsonPath});
%% ip6_address_port
convert_value(_ConfigDir, #json_type{name = ip6_address_port,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
	[Ip6AddressString, PortString] ->
            case inet:parse_ipv6_address(Ip6AddressString) of
                {ok, Ip6Address} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            transform_value(Transform, {Ip6Address, Port},
                                            JsonPath);
                        _ ->
                            throw({not_ip6_address_port, Value, JsonPath})
                    end;
                {error, einval} ->
                    throw({not_ip6_address_port, Value, JsonPath})
            end;
        _ ->
            throw({not_ip6_address_port, Value, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = ip6_address_port}, Value,
              JsonPath) ->
    throw({not_ip6_address_port, Value, JsonPath});
%% hostname_port
convert_value(_ConfigDir, #json_type{name = hostname_port,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
        [Hostname, PortString] ->
            case catch ?l2i(PortString) of
                Port when is_integer(Port) ->
                    transform_value(Transform, {Hostname, Port}, JsonPath);
                _ ->
                    throw({not_hostname_port, Value, JsonPath})
            end;
        _ ->
            throw({not_hostname_port, Value, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = hostname_port}, Value,
              JsonPath) ->
    throw({not_hostname_port, Value, JsonPath});
%% interface_port
convert_value(_ConfigDir, #json_type{name = interface_port,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
        [Interface, PortString] ->
            case get_ip_address(Interface) of
                {ok, Address} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            transform_value(Transform, {Address, Port}, JsonPath);
                        _ ->
                            throw({not_interface_port, Value, JsonPath})
                    end;
                {error, not_found} ->
                    throw({not_interface_port, Value, JsonPath})
            end;
        _ ->
            throw({not_interface_port3, Value, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = interface_port}, Value,
              JsonPath) ->
    throw({not_interface_port, Value, JsonPath});
%% base64
convert_value(_ConfigDir, #json_type{name = base64,
                                     transform = Transform}, Value, JsonPath)
  when is_binary(Value) ->
    transform_value(Transform, base64:decode(Value), JsonPath);
convert_value(_ConfigDir, #json_type{name = base64}, Value, JsonPath) ->
    throw({not_base64, Value, JsonPath});
%% readable_file
convert_value(ConfigDir, #json_type{name = readable_file, transform = Transform},
              Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = Type, access = Access}}
          when (Type == regular orelse Type == symlink) andalso
               (Access == read orelse Access == read_write) ->
            transform_value(Transform, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_readable_file, ExpandedFilename, JsonPath});
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = readable_file}, Value, JsonPath) ->
    throw({file_error, Value, einval, JsonPath});
%% writable_file
convert_value(ConfigDir, #json_type{name = writable_file, transform = Transform},
              Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = Type, access = read_write}}
          when Type == regular orelse Type == symlink ->
            transform_value(Transform, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_writable_file, _FileInfo, ExpandedFilename, JsonPath});
        {error, enoent} ->
            ParentDir = filename:dirname(ExpandedFilename),
            case file:read_file_info(ParentDir) of
                {ok, #file_info{type = directory, access = read_write}} ->
                    transform_value(Transform, ?l2b(ExpandedFilename), JsonPath);
                {ok, _FileInfo} ->
                    throw({not_writable_directory, ParentDir, JsonPath});
                {error, Reason} ->
                    throw({file_error, Value, Reason, JsonPath})
            end;
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = writable_file}, Value, JsonPath) ->
    throw({file_error, Value, einval, JsonPath});
%% readable_directory
convert_value(ConfigDir, #json_type{name = readable_directory,
                                    transform = Transform},
              Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = directory, access = read}} ->
            transform_value(Transform, ?l2b(ExpandedFilename), JsonPath);
        {ok, #file_info{type = directory, access = read_write}} ->
            transform_value(Transform, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_readable_directory, ExpandedFilename, JsonPath});
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
%% writable_directory
convert_value(ConfigDir, #json_type{name = writable_directory,
                                    transform = Transform},
              Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = directory, access = read_write}} ->
            transform_value(Transform, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_writable_directory, ExpandedFilename, JsonPath});
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
convert_value(_ConfigDir, #json_type{name = writable_directory}, Value,
              JsonPath) ->
    throw({file_error, Value, einval, JsonPath});
%% atom
convert_value(_ConfigDir, #json_type{name = atom, transform = Transform}, Value,
              JsonPath)
  when is_binary(Value) ->
    transform_value(Transform, ?b2a(Value), JsonPath);
convert_value(_ConfigDir, #json_type{name = atom}, Value, JsonPath) ->
    throw({not_atom, Value, JsonPath});
%% string
convert_value(_ConfigDir, #json_type{name = string, transform = Transform}, Value,
              JsonPath)
  when is_binary(Value) ->
    transform_value(Transform, Value, JsonPath);
convert_value(_ConfigDir, #json_type{name = string}, Value, JsonPath) ->
    throw({not_string, Value, JsonPath});
%% path
convert_value(ConfigDir, #json_type{name = path, transform = Transform}, Value,
              JsonPath)
  when is_binary(Value) ->
    Path = expand_config_dir(ConfigDir, ?b2l(Value)),
    transform_value(Transform, Path, JsonPath);
convert_value(_ConfigDir, #json_type{name = path}, Value, JsonPath) ->
    throw({not_string, Value, JsonPath}).

transform_value(undefined, Value, _JsonPath) ->
    Value;
transform_value(Transform, Value, JsonPath) ->
    case catch Transform(Value) of
        {failed, Reason} ->
            throw({invalid_transform_value, JsonPath, Reason});
        {'EXIT', _Reason} ->
            throw({invalid_value, Value, JsonPath});
        TransformedValue ->
            TransformedValue
    end.

expand_config_dir(_ConfigDir, []) ->
    [];
expand_config_dir(ConfigDir, "${CONFIG_DIR}" ++ Rest) ->
    ConfigDir ++ Rest;
expand_config_dir(ConfigDir, [C|Rest]) ->
    [C|expand_config_dir(ConfigDir, Rest)].

convert_values(_ConfigDir, _JsonType, [], _JsonPath) ->
    [];
convert_values(ConfigDir, JsonType, [JsonValue|Rest], JsonPath) ->
    [convert_value(ConfigDir, JsonType, JsonValue, JsonPath)|
     convert_values(ConfigDir, JsonType, Rest, JsonPath)].

get_ip_address("*") ->
    {ok, {0, 0, 0, 0}};
get_ip_address(IfName) ->
    {ok, IfAddrs} = inet:getifaddrs(),
    get_ip_address(IfName, IfAddrs).

get_ip_address(_IfName, []) ->
    {error, not_found};
get_ip_address(IfName, [{IfName, IfOpts}|_]) ->
    case lists:keysearch(addr, 1, IfOpts) of
        {value, {_, Addr}} ->
            {ok, Addr};
        false ->
            {error, not_found}
    end;
get_ip_address(IfName, [_|Rest]) ->
    get_ip_address(IfName, Rest).

%% Exported: unconvert_values

unconvert_values( _JsonType, []) ->
    [];
unconvert_values(JsonType, [JsonValue|Rest]) ->
    [unconvert_value(JsonType, JsonValue)|unconvert_values(JsonType, Rest)].

%% Exported: unconvert_value

unconvert_value(#json_type{untransform = Untransform} = JsonType, Value)
  when Untransform /= undefined ->
    unconvert_value(JsonType#json_type{untransform = undefined}, Untransform(Value));
unconvert_value(#json_type{name = bool}, Value) ->
    Value;
unconvert_value(#json_type{name = {integer, _, _}}, Value) ->
    Value;
unconvert_value(#json_type{name = {float, _, _}}, Value) ->
    Value;
unconvert_value(#json_type{name = Name}, Value)
  when Name == ip_address orelse
       Name == ip4_address orelse
       Name == ip6_address ->
    ?l2b(inet_parse:ntoa(Value));
unconvert_value(#json_type{name = Name}, {IpAddress, Port})
  when Name == ip_address_port orelse
       Name == ip4_address_port orelse
       Name == ip6_address_port ->
    ?l2b(io_lib:format("~s:~w", [inet_parse:ntoa(IpAddress), Port]));
unconvert_value(#json_type{name = hostname_port}, {Hostname, Port}) ->
    ?l2b(io_lib:format("~s:~w", [Hostname, Port]));
unconvert_value(#json_type{name = interface_port}, {IpAddress, Port}) ->
    {ok, Interface} = get_interface(IpAddress),
    ?l2b(io_lib:format("~s:~w", [Interface, Port]));
unconvert_value(#json_type{name = base64}, Value) ->
    base64:encode(Value);
unconvert_value(#json_type{name = Name}, Value)
  when Name == readable_file orelse
       Name == writable_file orelse
       Name == readable_directory orelse
       Name == writable_directory ->
    Value;
unconvert_value(#json_type{name = atom}, Value) ->
    ?a2b(Value);
unconvert_value(#json_type{name = string}, Value) ->
    Value;
unconvert_value(#json_type{name = path}, Value) ->
    Value.

get_interface({0, 0, 0, 0}) ->
    {ok, "*"};
get_interface(IpAddress) ->
    {ok, IfAddrs} = inet:getifaddrs(),
    get_interface(IpAddress, IfAddrs).

get_interface(_IpAddress, []) ->
    {error, not_found};
get_interface(IpAddress, [{IfName, IfOpts}|Rest]) ->
    case lists:keysearch(addr, 1, IfOpts) of
        {value, {_, IpAddress}} ->
            {ok, IfName};
        _ ->
            get_interface(IpAddress, Rest)
    end.

%% Exported: format_error

-spec format_error(error_reason()) -> binary().

format_error(already_started) ->
    <<"Already started">>;
format_error({posix, Reason}) ->
    ?l2b(inet:format_error(Reason));
format_error({config, {bad_json, Reason}}) ->
    %%    <<"Syntax error">>;
    ?l2b(io_lib:format("Bad JSON: ~p", [Reason]));
format_error({config, {file_error, Filename, Reason}}) ->
    ?l2b(io_lib:format("~s: ~s", [Filename, file:format_error(Reason)]));
format_error({config, {file_error, Filename, Reason, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s: ~s",
                       [json_path_to_string(JsonPath), Filename,
                        file:format_error(Reason)]));
format_error({config, {missing, Name}}) ->
    ?l2b(io_lib:format("\"~s\" is missing", [Name]));
format_error({config, {unexpected, Name}}) ->
    ?l2b(io_lib:format("\"~s\" was not expected", [Name]));
format_error({config, {expected, ExpectedJsonPath, JsonPath}}) ->
    ?l2b(io_lib:format("Expected \"~s\", got \"~s\"",
                       [json_path_to_string(ExpectedJsonPath),
                        json_path_to_string(JsonPath)]));
format_error({config, {not_bool, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid boolean value",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {integer_out_of_range, Value, From, To, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s must be in the range between ~w and ~w",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value), From, To]));
format_error({config, {not_integer, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid integer",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {float_out_of_range, Value, From, To, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s must be in the range between ~w and ~w",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value), From, To]));
format_error({config, {not_float, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid float",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ip_address, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip-address",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ip_address_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip-address and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ip4_address, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip4-address",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ip4_address_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip4-address and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_hostname_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid hostname and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_interface_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid interface and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ip6_address, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip6-address",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ip6_address_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip6-address and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_base64, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid base64 value",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_readable_file, Dir, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not readable",
                       [json_path_to_string(JsonPath), Dir]));
format_error({config, {not_writable_file, Dir, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not writable",
                       [json_path_to_string(JsonPath), Dir]));
format_error({config, {not_readable_directory, Dir, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is readable",
                       [json_path_to_string(JsonPath), Dir]));
format_error({config, {not_writable_directory, Dir, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not writable",
                       [json_path_to_string(JsonPath), Dir]));
format_error({config, {not_atom, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid atom",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_string, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid string",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {invalid_value, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid value",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {invalid_transform_value, JsonPath, Reason}}) ->
    ?l2b(io_lib:format("~s: ~s", [json_path_to_string(JsonPath), Reason]));
format_error(UnknownReason) ->
    error_logger:error_report(
      {?MODULE, ?LINE, {unknown_message, UnknownReason}}),
    <<"Internal error">>.

json_value_to_string(JsonValue) when is_binary(JsonValue) ->
    lists:flatten(io_lib:format("~s", [JsonValue]));
json_value_to_string(JsonValue) ->
    lists:flatten(io_lib:format("~p", [JsonValue])).

%%
%% Server
%%

init(Parent, ConfigFilename, AppSchemas, ReadConfig, Handler) ->
    ?MODULE = ets:new(?MODULE, [public, named_table]),
    RunningConfigFilename = ConfigFilename ++ "-running",
    case filelib:is_regular(RunningConfigFilename) of
        true ->
            ok;
        false ->
            {ok, _} = file:copy(ConfigFilename, RunningConfigFilename)
    end,
    case load_config_file(RunningConfigFilename, AppSchemas) of
        true ->
            {IpAddress, Port} = ReadConfig(),
            TcpServ =
                proc_lib:spawn_link(
                  fun() ->
                          {ok, ListenSocket} =
                              gen_tcp:listen(Port,
                                             [{packet, 2},
                                              {ip, IpAddress},
                                              binary,
                                              {reuseaddr, true}]),
                          listener(ListenSocket, Handler)
                  end),
            {ok, #state{parent = Parent,
                        tcp_serv = TcpServ,
                        config_filename = RunningConfigFilename,
                        app_schemas = AppSchemas}};
        {error, Reason} ->
            {error, {config, Reason}}
    end.

load_config_file(ConfigFilename, AppSchemas) ->
    case file:read_file(ConfigFilename) of
        {ok, EncodedJson} ->
            case jsone:try_decode(EncodedJson, [{object_format, proplist}]) of
                {ok, JsonTerm, _} ->
                    try
                        ConfigDir = filename:dirname(ConfigFilename),
                        AtomifiedJsonTerm = atomify(JsonTerm),
                        load_json_term(
                          ConfigDir, AppSchemas, AtomifiedJsonTerm)
                    catch
                        throw:Reason ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {bad_json, Reason}}
            end;
        {error, Reason} ->
            {error, {file_error, ConfigFilename, Reason}}
    end.

listener(ListenSocket, Handler) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    ok = Handler(Socket),
    ok = gen_tcp:close(Socket),
    listener(ListenSocket, Handler).

message_handler(#state{parent = Parent,
                       tcp_serv = TcpServ,
                       config_filename = ConfigFilename,
                       app_schemas = AppSchemas,
                       subscribers = Subscribers} = S) ->
    receive
        {cast, {subscribe, ClientPid}} ->
            case lists:member(ClientPid, Subscribers) of
                true ->
                    noreply;
                false ->
                    erlang:monitor(process, ClientPid),
                    {noreply, S#state{subscribers = [ClientPid|Subscribers]}}
            end;
        {call, From, export_config_file} ->
            ok = replace_config_file(ConfigFilename, AppSchemas),
            {reply, From, ok};
        {'DOWN', _MonitorRef, process, ClientPid, _Info} ->
            UpdatedSubscribers = lists:delete(ClientPid, Subscribers),
            {noreply, S#state{subscribers = UpdatedSubscribers}};
        reload ->
	    %% Ensure that reload is called in all applications
	    EnvBefore = application_controller:prep_config_change(),
            case load_config_file(ConfigFilename, AppSchemas) of
                true ->
                    ok = application_controller:config_change(EnvBefore),
                    %% Inform all subscribers
                    lists:foreach(fun(ClientPid) ->
                                          ClientPid ! config_updated
                                  end, Subscribers),
                    {noreply, S};
                {error, _Reason} ->
                    noreply
            end;
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {'EXIT', TcpServ, Reason} ->
            exit(Reason);
        {system, From, Request} ->
            {system, From, Request};
        UnknownMessage ->
            error_logger:error_report(
              {?MODULE, ?LINE, {unknown_message, UnknownMessage}}),
            noreply
    end.

replace_config_file(ConfigFilename, AppSchemas) ->            
    JsonTerm = unconvert(AppSchemas),
    {ok, Binary} =
        jsone:try_encode(JsonTerm,
                         [{float_format, [{decimals, 4}, compact]},
                          {indent, 2},
                          {object_key_type, value},
                          {space, 1},
                          native_forward_slash]),
    file:write_file(ConfigFilename, Binary).

unconvert([]) ->
    [];
unconvert([{App, [{Name, _JsonType}|_] = Schema}|Rest]) ->
    {ok, JsonTerm} = application:get_env(App, Name),
    unconvert(Schema, JsonTerm) ++ unconvert(Rest).

unconvert(Schema, JsonTerm) ->
    unconvert(Schema, JsonTerm, []).

unconvert([], [], UnconvertedJsonTerm) ->
    lists:reverse(UnconvertedJsonTerm);
%% Single value
unconvert([{Name, JsonType}|SchemaRest],
          [{Name, JsonValue}|JsonTermRest], UnconvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    UnconvertedValue = unconvert_value(JsonType, JsonValue),
    unconvert(SchemaRest, JsonTermRest,
              [{Name, UnconvertedValue}|UnconvertedJsonTerm]);
%% Array of single values
unconvert([{Name, [JsonType]}|SchemaRest],
          [{Name, JsonValues}|JsonTermRest], UnconvertedJsonTerm)
  when is_record(JsonType, json_type) ->
    UnconvertedValues = unconvert_values(JsonType, JsonValues),
    unconvert(SchemaRest, JsonTermRest,
              [{Name, UnconvertedValues}|UnconvertedJsonTerm]);
%% Object
unconvert([{Name, NestedSchema}|SchemaRest],
          [{Name, NestedJsonTerm}|JsonTermRest], UnconvertedJsonTerm) ->
    UnconvertedNestedJsonTerm = unconvert(NestedSchema, NestedJsonTerm),
    unconvert(SchemaRest, JsonTermRest,
              [{Name, UnconvertedNestedJsonTerm}|UnconvertedJsonTerm]);
%% List
unconvert([Schema|SchemaRest], [JsonTerm|JsonTermRest], UnconvertedJsonTerm)
  when is_list(Schema), is_list(JsonTerm) ->
    UnconvertedFirstJsonTerm = unconvert(Schema, JsonTerm),
    unconvert([Schema|SchemaRest], JsonTermRest,
              UnconvertedFirstJsonTerm ++ UnconvertedJsonTerm).
