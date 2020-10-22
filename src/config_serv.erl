-module(config_serv).
-export([start_link/4]).
-export([subscribe/0, subscribe/1]).
-export([json_lookup/2]).
-export([tcp_send/3]).
-export([format_error/1]).
-export_type([type_name/0, json_path/0, json_term/0, json_value/0,
              error_reason/0]).

-include_lib("kernel/include/file.hrl").
-include("../include/config_schema.hrl").
-include("../include/shorthand.hrl").
-include("../include/serv.hrl").

-define(DEFAULT_CONTROL_ADDRESS, {127, 0, 0, 1}).
-define(DEFAULT_CONTROL_PORT, 23313).
-define(MAX_SESSIONS, 8).

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
	ipaddress |
	ipaddress_port |
        ipv4address |
        ipv4address_port |
        hostname_port |
        ipv6address |
        base64 |
        readable_file |
        writable_file |
        readable_directory |
        writable_directory |
        atom |
        string |
        path.
-type ip4_address_port() :: {inet:ip4_address(), inet:port_number()}.
-type ip6_address_port() :: {inet:ip6_address(), inet:port_number()}.
-type ip_address_port() :: {inet:ip_address(), inet:port_number()}.
-type hostname_port() :: {string(), inet:port_number()}.
-type enum() :: atom().
-type json_value() :: inet:ip_address() |
                      ip_address_port() |
		      ip4_address_port() |
		      ip6_address_port() |
                      hostname_port() |
                      enum() |
                      integer() |
                      boolean() |
                      atom() |
                      binary() |
                      null |
                      float().
-type json_term() ::
        [{binary() | atom(), json_term()}] |
        [json_term()] |
        json_value().

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
        {not_ipaddress, json_value(), json_path()} |
	{not_ipaddress_port, json_value(), json_path()} |
        {not_ipv4address, json_value(), json_path()} |
        {not_ipv4address_port, json_value(), json_path()} |
        {not_hostname_port, json_value(), json_path()} |
        {not_ipv6address, json_value(), json_path()} |
        {not_ipv6address_port, json_value(), json_path()} |
        {not_base64, json_value(), json_path()} |
        {not_readable_file, string(), json_path()} |
        {not_writable_file, string(), json_path()} |
        {not_readable_directory, string(), json_path()} |
        {not_writable_directory, string(), json_path()} |
        {not_atom, json_value(), json_path()} |
        {not_string, json_value(), json_path()} |
        {invalid_value, json_value(), json_path()} |
        {invalid_convert_value, json_path(), string()}.

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
       fun message_handler/1,
       #serv_options{name = ?MODULE}).

%% Exported: subscribe

-spec subscribe(pid()) -> ok.

subscribe() ->
    serv:cast(?MODULE, {subscribe, self()}).

subscribe(Pid) ->
    serv:cast(?MODULE, {subscribe, Pid}).

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
format_error({config, {not_ipaddress, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip-address",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ipaddress_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ip-address and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ipv4address, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ipv4-address",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ipv4address_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ipv4-address and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_hostname_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid hostname and port",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ipv6_address, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ipv6-address",
                       [json_path_to_string(JsonPath),
                        json_value_to_string(Value)]));
format_error({config, {not_ipv6address_port, Value, JsonPath}}) ->
    ?l2b(io_lib:format("~s: ~s is not a valid ipv6-address and port",
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
format_error({config, {invalid_convert_value, JsonPath, Reason}}) ->
    ?l2b(io_lib:format("~s: ~s", [json_path_to_string(JsonPath), Reason]));
format_error(UnknownReason) ->
    error_logger:error_report(
      {?MODULE, ?LINE, {unknown_message, UnknownReason}}),
    <<"Internal error">>.

json_path_to_string([Name|Rest]) ->
    json_path_to_string(Rest, [?a2l(Name)]).

json_path_to_string([], Acc) ->
    Acc;
json_path_to_string([Name|Rest], Acc) ->
    json_path_to_string(Rest, [?a2l(Name), $/|Acc]).

json_value_to_string(JsonValue) when is_integer(JsonValue) ->
    ?i2l(JsonValue);
json_value_to_string(JsonValue) when is_float(JsonValue) ->
    ?f2l(JsonValue);
json_value_to_string(true) ->
    "true";
json_value_to_string(false) ->
    "false";
json_value_to_string(JsonValue) when is_atom(JsonValue) ->
    ?a2l(JsonValue);
json_value_to_string(JsonValue) when is_binary(JsonValue) ->
    ?b2l(JsonValue);
json_value_to_string({{_I1, _I2, _I3, _I4} = Ip4Address, Port}) ->
    [inet_parse:ntoa(Ip4Address), ":", ?i2l(Port)];
json_value_to_string({Hostname, Port}) when is_list(Hostname) ->
    [Hostname, ":", ?i2l(Port)].

%%
%% Server
%%

init(Parent, ConfigFilename, AppSchemas, ReadConfig, Handler) ->
    ?MODULE = ets:new(?MODULE, [public, named_table]),
    case load_config_file(ConfigFilename, AppSchemas) of
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
                        config_filename = ConfigFilename,
                        app_schemas = AppSchemas}};
        {error, Reason} ->
            {error, {config, Reason}}
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
        {'DOWN', _MonitorRef, process, ClientPid, _Info} ->
            UpdatedSubscribers = lists:delete(ClientPid, Subscribers),
            {noreply, S#state{subscribers = UpdatedSubscribers}};
        reload ->
            case load_config_file(ConfigFilename, AppSchemas) of
                true ->
                    %% Ensure that reload is called in all applications
                    EnvBefore = application_controller:prep_config_change(),
                    ok = application_controller:config_change(EnvBefore),
                    %% Inform all subscribers
                    lists:foreach(fun(ClientPid) ->
                                          ClientPid ! config_updated
                                  end, Subscribers),
                    {noreply, S};
                {error, _Reason} ->
                    noreply
            end;
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        {'EXIT', TcpServ, Reason} ->
            exit(Reason);
        UnknownMessage ->
            error_logger:error_report(
              {?MODULE, ?LINE, {unknown_message, UnknownMessage}}),
            noreply
    end.

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

%%
%% Load config file
%%

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
    case validate(ConfigDir, Schema, JsonTerm) of
        {ValidatedJsonTerm, []} when RemainingAppSchemas == [] ->
            ok = application:set_env(
                   App, FirstNameInJsonPath, ValidatedJsonTerm,
                   [{persistent, true}]),
            true = ets:insert(?MODULE, {FirstNameInJsonPath, App});
        {_ValidatedJsonTerm, []} ->
            [{_App, [{Name, _JsonTerm}|_]}|_] = RemainingAppSchemas,
            throw({missing, Name});
        {ValidatedJsonTerm, RemainingJsonTerm} ->
            ok = application:set_env(
                   App, FirstNameInJsonPath, ValidatedJsonTerm,
                   [{persistent, true}]),
            true = ets:insert(?MODULE, {FirstNameInJsonPath, App}),
            load_json_term(ConfigDir, RemainingAppSchemas,
                           RemainingJsonTerm)
    end.

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

validate(ConfigDir, Schema, JsonTerm) ->
    validate(ConfigDir, Schema, JsonTerm, [], []).

validate(_ConfigDir, [], RemainingJsonTerm, _JsonPath, ValidatedJsonTerm) ->
    {lists:reverse(ValidatedJsonTerm), RemainingJsonTerm};
%% Single value
validate(ConfigDir,
         [{Name, JsonType}|SchemaRest],
         [{Name, JsonValue}|JsonTermRest], JsonPath,
         ValidatedJsonTerm)
  when is_record(JsonType, json_type) ->
    ValidatedValue =
        validate_value(ConfigDir, JsonType, JsonValue, [Name|JsonPath]),
    validate(ConfigDir, SchemaRest, JsonTermRest, JsonPath,
             [{Name, ValidatedValue}|ValidatedJsonTerm]);
validate(_ConfigDir,
         [{Name, JsonType}|_SchemaRest],
         [{AnotherName, _JsonValue}|_JsonTermRest], JsonPath,
         _ValidatedJsonTerm)
  when is_record(JsonType, json_type) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
%% Array of single values
validate(ConfigDir,
         [{Name, [JsonType]}|SchemaRest],
         [{Name, JsonValues}|JsonTermRest], JsonPath,
         ValidatedJsonTerm)
  when is_record(JsonType, json_type) ->
    ValidatedValues =
        validate_values(ConfigDir, JsonType, JsonValues, [Name|JsonPath]),
    validate(ConfigDir, SchemaRest, JsonTermRest, JsonPath,
             [{Name, ValidatedValues}|ValidatedJsonTerm]);
validate(_ConfigDir,
         [{Name, [JsonType]}|_SchemaRest],
         [{AnotherName, _JsonValue}|_JsonTermRest], JsonPath,
         _ValidatedJsonTerm)
  when is_record(JsonType, json_type) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
%% Object
validate(ConfigDir,
         [{Name, NestedSchema}|SchemaRest],
         [{Name, NestedJsonTerm}|JsonTermRest], JsonPath,
         ValidatedJsonTerm) ->
    {ValidatedNestedJsonTerm, []} =
        validate(ConfigDir, NestedSchema, NestedJsonTerm, [Name|JsonPath], []),
    validate(ConfigDir, SchemaRest, JsonTermRest, JsonPath,
             [{Name, ValidatedNestedJsonTerm}|ValidatedJsonTerm]);
%% Mismatch
validate(_ConfigDir,
         [{Name, _NestedSchema}|_SchemaRest],
         [{AnotherName, _NestedJsonTerm}|_JsonTermRest], JsonPath,
         _ValidatedJsonTerm) ->
    throw({expected, [Name|JsonPath], [AnotherName|JsonPath]});
%% List
validate(ConfigDir,
         [Schema|SchemaRest],
         [JsonTerm|JsonTermRest], JsonPath,
         ValidatedJsonTerm)
  when is_list(Schema), is_list(JsonTerm) ->
    {ValidatedFirstJsonTerm, []} =
        validate(ConfigDir, Schema, JsonTerm, JsonPath, []),
    validate(ConfigDir, [Schema|SchemaRest], JsonTermRest, JsonPath,
             ValidatedFirstJsonTerm ++ ValidatedJsonTerm).

%% bool
validate_value(_ConfigDir, #json_type{name = bool, convert = Convert}, Value,
               JsonPath)
  when is_boolean(Value) ->
    convert_value(Convert, Value, JsonPath);
validate_value(_ConfigDir, #json_type{name = bool}, Value, JsonPath) ->
    throw({not_bool, Value, JsonPath});
%% integer
validate_value(_ConfigDir, #json_type{name = {integer, From, unbounded},
                                      convert = Convert},
               Value, JsonPath)
  when is_integer(Value) andalso Value >= From andalso Value >= From ->
    convert_value(Convert, Value, JsonPath);
validate_value(_ConfigDir, #json_type{name = {integer, From, To},
                                      convert = Convert}, Value,
               JsonPath)
  when is_integer(Value) andalso Value >= From andalso Value =< To ->
    convert_value(Convert, Value, JsonPath);
validate_value(_ConfigDir, #json_type{name = {integer, From, To}}, Value,
               JsonPath)
  when is_integer(Value) ->
    throw({integer_out_of_range, Value, From, To, JsonPath});
validate_value(_ConfigDir, #json_type{name = {integer, _From, _To}}, Value,
               JsonPath) ->
    throw({not_integer, Value, JsonPath});
%% float
validate_value(_ConfigDir, #json_type{name = {float, From, unbounded},
                                      convert = Convert},
               Value, JsonPath)
  when is_float(Value) andalso Value >= From andalso Value >= From ->
    convert_value(Convert, Value, JsonPath);
validate_value(_ConfigDir, #json_type{name = {float, From, To},
                                      convert = Convert}, Value,
               JsonPath)
  when is_float(Value) andalso Value >= From andalso Value =< To ->
    convert_value(Convert, Value, JsonPath);
validate_value(_ConfigDir, #json_type{name = {float, From, To}}, Value,
               JsonPath)
  when is_float(Value) ->
    throw({float_out_of_range, Value, From, To, JsonPath});
validate_value(_ConfigDir, #json_type{name = {float, _From, _To}}, Value,
               JsonPath) ->
    throw({not_float, Value, JsonPath});
%% ipaddress
validate_value(_ConfigDir, #json_type{name = ipaddress,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    case inet:parse_address(?b2l(Value)) of
        {ok, IpAddress} ->
            convert_value(Convert, IpAddress, JsonPath);
        {error, einval} ->
            throw({not_ipaddress, Value, JsonPath})
    end;

%% ipaddress_port 1.2.3.4:80 or [10:11:12:13::abcd]:80
validate_value(_ConfigDir, #json_type{name = ipaddress_port,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    case string:rchr(?b2l(Value), $:) of
	0 ->
	    throw({not_ipaddress_port, Value, JsonPath});
	Pos ->
	    %% [1:2:3:4]:80  Pos-1 = [1:2:3:4] Pos-3 = 1:2:3:4
	    Size6 = Pos-3,
	    case split_binary(Value, Pos-1) of
		{<<$[,IpValue:Size6/binary,$]>>,<<$:,PortValue/binary>>} ->
		    case inet:parse_ipv6_address(?b2l(IpValue)) of
			{ok, Ipv6Address} ->
			    case catch ?b2i(PortValue) of
				Port when is_integer(Port) ->
				    convert_value(Convert, {Ipv6Address, Port},
						  JsonPath);
				_ ->
				    throw({not_ipaddress_port, Value, JsonPath})
			    end;
			{error, einval} ->
			    throw({not_ipaddress_port, Value, JsonPath})
		    end;
		{IpValue,<<$:,PortValue/binary>>} ->
		    case inet:parse_ipv4_address(?b2l(IpValue)) of
			{ok, Ipv4Address} ->
			    case catch ?b2i(PortValue) of
				Port when is_integer(Port) ->
				    convert_value(Convert, {Ipv4Address, Port},
						  JsonPath);
				_ ->
				    throw({not_ipaddress_port, Value, JsonPath})
			    end;
			{error, einval} ->
			    throw({not_ipaddress, Value, JsonPath})
		    end;
		_ ->
		    throw({not_ipaddress_port, Value, JsonPath})
	    end
    end;
validate_value(_ConfigDir, #json_type{name = ipaddress_port}, Value,
               JsonPath) ->
    throw({not_ipaddress_port, Value, JsonPath});
%% ipv4address
validate_value(_ConfigDir, #json_type{name = ipv4address,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    case inet:parse_ipv4_address(?b2l(Value)) of
        {ok, Ipv4Address} ->
            convert_value(Convert, Ipv4Address, JsonPath);
        {error, einval} ->
            throw({not_ipv4_address, Value, JsonPath})
    end;
%% ipv4address_port
validate_value(_ConfigDir, #json_type{name = ipv4address_port,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
        [Ipv4AddressString, PortString] ->
            case inet:parse_ipv4_address(Ipv4AddressString) of
                {ok, Ipv4Address} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            convert_value(Convert, {Ipv4Address, Port},
                                          JsonPath);
                        _ ->
                            throw({not_ipv4address_port, Value, JsonPath})
                    end;
                {error, einval} ->
                    throw({not_ipv4address_port, Value, JsonPath})
            end;
        _ ->
            throw({not_ipv4_address_port, Value, JsonPath})
    end;
validate_value(_ConfigDir, #json_type{name = ipv4address_port}, Value,
               JsonPath) ->
    throw({not_ipv4address_port, Value, JsonPath});
%% hostname_port
validate_value(_ConfigDir, #json_type{name = hostname_port,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
        [Hostname, PortString] ->
            case catch ?l2i(PortString) of
                Port when is_integer(Port) ->
                    convert_value(Convert, {Hostname, Port}, JsonPath);
                _ ->
                    throw({not_hostname_port, Value, JsonPath})
            end;
        _ ->
            throw({not_hostname_port, Value, JsonPath})
    end;
validate_value(_ConfigDir, #json_type{name = hostname_port}, Value,
               JsonPath) ->
    throw({not_hostname_port, Value, JsonPath});
%% ipv6address
validate_value(_ConfigDir, #json_type{name = ipv6address,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    case inet:parse_ipv6_address(?b2l(Value)) of
        {ok, Ipv6Address} ->
            convert_value(Convert, Ipv6Address, JsonPath);
        {error, einval} ->
            throw({not_ipv6_address, Value, JsonPath})
    end;
validate_value(_ConfigDir, #json_type{name = ipv6address}, Value, JsonPath) ->
    throw({not_ipv6_address, Value, JsonPath});
%% ipv6address_port
validate_value(_ConfigDir, #json_type{name = ipv6address_port,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    case string:tokens(?b2l(Value), ":") of
	[Ipv6AddressString, PortString] ->
            case inet:parse_ipv6_address(Ipv6AddressString) of
                {ok, Ipv6Address} ->
                    case catch ?l2i(PortString) of
                        Port when is_integer(Port) ->
                            convert_value(Convert, {Ipv6Address, Port},
                                          JsonPath);
                        _ ->
                            throw({not_ipv6address_port, Value, JsonPath})
                    end;
                {error, einval} ->
                    throw({not_ipv6address_port, Value, JsonPath})
            end;
        _ ->
            throw({not_ipv6_address_port, Value, JsonPath})
    end;
validate_value(_ConfigDir, #json_type{name = ipv6address_port}, Value,
               JsonPath) ->
    throw({not_ipv6address_port, Value, JsonPath});
%% base64
validate_value(_ConfigDir, #json_type{name = base64,
                                      convert = Convert}, Value, JsonPath)
  when is_binary(Value) ->
    convert_value(Convert, base64:decode(Value), JsonPath);
validate_value(_ConfigDir, #json_type{name = base64}, Value, JsonPath) ->
    throw({not_base64, Value, JsonPath});
%% readable_file
validate_value(ConfigDir, #json_type{name = readable_file, convert = Convert},
               Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = Type, access = Access}}
          when (Type == regular orelse Type == symlink) andalso
               (Access == read orelse Access == read_write) ->
            convert_value(Convert, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_readable_file, ExpandedFilename, JsonPath});
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
validate_value(_ConfigDir, #json_type{name = readable_file}, Value, JsonPath) ->
    throw({file_error, Value, einval, JsonPath});
%% writable_file
validate_value(ConfigDir, #json_type{name = writable_file, convert = Convert},
               Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = Type, access = read_write}}
          when Type == regular orelse Type == symlink ->
            convert_value(Convert, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_writable_file, _FileInfo, ExpandedFilename, JsonPath});
        {error, enoent} ->
            ParentDir = filename:dirname(ExpandedFilename),
            case file:read_file_info(ParentDir) of
                {ok, #file_info{type = directory, access = read_write}} ->
                    convert_value(Convert, ?l2b(ExpandedFilename), JsonPath);
                {ok, _FileInfo} ->
                    throw({not_writable_directory, ParentDir, JsonPath});
                {error, Reason} ->
                    throw({file_error, Value, Reason, JsonPath})
            end;
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
validate_value(_ConfigDir, #json_type{name = writable_file}, Value, JsonPath) ->
    throw({file_error, Value, einval, JsonPath});
%% readable_directory
validate_value(ConfigDir, #json_type{name = readable_directory,
                                     convert = Convert},
               Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = directory, access = read}} ->
            convert_value(Convert, ?l2b(ExpandedFilename), JsonPath);
        {ok, #file_info{type = directory, access = read_write}} ->
            convert_value(Convert, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_readable_directory, ExpandedFilename, JsonPath});
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
%% writable_directory
validate_value(ConfigDir, #json_type{name = writable_directory,
                                     convert = Convert},
               Value, JsonPath)
  when is_binary(Value) ->
    ExpandedFilename = expand_config_dir(ConfigDir, ?b2l(Value)),
    case file:read_file_info(ExpandedFilename) of
        {ok, #file_info{type = directory, access = read_write}} ->
            convert_value(Convert, ?l2b(ExpandedFilename), JsonPath);
        {ok, _FileInfo} ->
            throw({not_writable_directory, ExpandedFilename, JsonPath});
        {error, Reason} ->
            throw({file_error, Value, Reason, JsonPath})
    end;
validate_value(_ConfigDir, #json_type{name = writable_directory}, Value,
               JsonPath) ->
    throw({file_error, Value, einval, JsonPath});
%% atom
validate_value(_ConfigDir, #json_type{name = atom, convert = Convert}, Value,
               JsonPath)
  when is_binary(Value) ->
    convert_value(Convert, ?b2a(Value), JsonPath);
validate_value(_ConfigDir, #json_type{name = atom}, Value, JsonPath) ->
    throw({not_atom, Value, JsonPath});
%% string
validate_value(_ConfigDir, #json_type{name = string, convert = Convert}, Value,
               JsonPath)
  when is_binary(Value) ->
    convert_value(Convert, Value, JsonPath);
validate_value(_ConfigDir, #json_type{name = string}, Value, JsonPath) ->
    throw({not_string, Value, JsonPath});
%% path
validate_value(ConfigDir, #json_type{name = path, convert = Convert}, Value,
               JsonPath)
  when is_binary(Value) ->
    Path = expand_config_dir(ConfigDir, ?b2l(Value)),
    convert_value(Convert, Path, JsonPath);
validate_value(_ConfigDir, #json_type{name = path}, Value, JsonPath) ->
    throw({not_string, Value, JsonPath}).

convert_value(undefined, Value, _JsonPath) ->
    Value;
convert_value(Convert, Value, JsonPath) ->
    case catch Convert(Value) of
        {failed, Reason} ->
            throw({invalid_convert_value, JsonPath, Reason});
        {'EXIT', _Reason} ->
            throw({invalid_value, Value, JsonPath});
        ConvertedValue ->
            ConvertedValue
    end.

expand_config_dir(_ConfigDir, []) ->
    [];
expand_config_dir(ConfigDir, "${CONFIG_DIR}" ++ Rest) ->
    ConfigDir ++ Rest;
expand_config_dir(ConfigDir, [C|Rest]) ->
    [C|expand_config_dir(ConfigDir, Rest)].

validate_values(_ConfigDir, _JsonType, [], _JsonPath) ->
    [];
validate_values(ConfigDir, JsonType, [JsonValue|Rest], JsonPath) ->
    [validate_value(ConfigDir, JsonType, JsonValue, JsonPath)|
     validate_values(ConfigDir, JsonType, Rest, JsonPath)].
