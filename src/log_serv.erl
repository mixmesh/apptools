-module(log_serv).
-export([start_link/2]).
-export([is_log_enabled/1]).
-export([daemon_log/6]).
-export([dbg_log/5]).
-export([format_error/1]).
-export_type([error_reason/0]).

-include("../include/log_serv.hrl").
-include("../include/shorthand.hrl").
-include("../include/serv.hrl").

-record(state,
        {parent :: pid(),
         tty_available :: boolean(),
         read_config :: read_config(),
         daemon_log_info :: #daemon_log_info{},
         %% Fallback to any(), i.e. disk_log:log() type is not exported
         daemon_disk_log :: any(),
         dbg_log_info :: #dbg_log_info{},
         %% Fallback to any(), i.e. disk_log:log() type is not exported
         dbg_disk_log :: any(),
         error_log_info :: #error_log_info{},
         disabled_processes = [] :: [pid()]}).

-type read_config() ::
        fun(() -> {#daemon_log_info{}, #dbg_log_info{}, #error_log_info{}}).
-type error_reason() :: already_started | term().

%% Exported: start_link

-spec start_link(atom(), read_config()) ->
          serv:spawn_server_result() | {error, error_reason()}.

start_link(ConfigServ, ReadConfig) ->
    ?spawn_server_opts(
       fun(Parent) ->
               init(Parent, ConfigServ, ReadConfig, tty_available())
       end,
       fun message_handler/1,
       #serv_options{name = ?MODULE}).

%% Exported: is_log_enabled

is_log_enabled(LogType) ->
    [{LogType, Enabled}] = ets:lookup(?MODULE, LogType),
    Enabled.

%% Exported: daemon_log

-spec daemon_log(pid(), Module :: atom(), Tag :: atom() | [atom()],
                 Line :: integer(), Format :: string(), Args :: [any()]) ->
                        ok.

daemon_log(Pid, Module, Tag, Line, Format, Args) ->
    serv:cast(?MODULE, {daemon_log, Pid, Module, Tag, Line, Format, Args}).

%% Exported: dbg_log

-spec dbg_log(pid(), Module :: atom(), Tag :: atom() | [atom()],
              Line :: integer(), term()) -> ok.

dbg_log(Pid, Module, Tag, Line, Term) ->
    serv:cast(?MODULE, {dbg_log, Pid, Module, Tag, Line, Term}).

%% Exported: format_error

-spec format_error(error_reason()) -> iolist().

format_error(already_started) ->
    "Already started";
format_error(Reason) ->
    disk_log:format_error(Reason).

%%
%% Server
%%

init(Parent, ConfigServ, ReadConfig, TtyAvailable) ->
    {DaemonLogInfo, DbgLogInfo, ErrorLogInfo} = ReadConfig(),
    case open_log(DaemonLogInfo) of
        {ok, DaemonDiskLog} ->
            case open_log(DbgLogInfo) of
                {ok, DbgDiskLog} ->
                    case ErrorLogInfo of
                        #error_log_info{enabled = true,
                                        file = {true, Filename}} ->
                            ok = error_logger:add_report_handler(
                                   log_mf_h,
                                   log_mf_h:init(?b2l(Filename),
                                                 1024*1024*1024, 2,
                                                 fun({error, _, _}) ->
                                                         true;
                                                    ({error_report, _, _}) ->
                                                         true;
                                                    (_) ->
                                                         false
                                                 end));
                        _ ->
                            ok
                    end,
                    ok = config_serv:subscribe(ConfigServ),
                    ?MODULE = ets:new(?MODULE, [public, named_table]),
                    true = save_enable_state(
                             DaemonLogInfo, DbgLogInfo, ErrorLogInfo),
                    {ok, #state{parent = Parent,
                                tty_available = TtyAvailable,
                                read_config = ReadConfig,
                                daemon_log_info = DaemonLogInfo,
                                daemon_disk_log = DaemonDiskLog,
                                dbg_log_info = DbgLogInfo,
                                dbg_disk_log = DbgDiskLog,
                                error_log_info = ErrorLogInfo}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

message_handler(#state{parent = Parent,
                       tty_available = TtyAvailable,
                       read_config = ReadConfig,
                       daemon_log_info = DaemonLogInfo,
                       daemon_disk_log = DaemonDiskLog,
                       dbg_log_info = DbgLogInfo,
                       dbg_disk_log = DbgDiskLog,
                       error_log_info = _ErrorLogInfo,
                       disabled_processes = DisabledProcesses} = S) ->
    receive
        {cast, {daemon_log, Pid, Module, Tag, Line, Format, Args}} ->
            case lists:member(Pid, DisabledProcesses) of
                false ->
                    write_to_daemon_log(
                      TtyAvailable, DaemonLogInfo, DaemonDiskLog, Pid, Module,
                      Tag, Format, Args),
                    write_to_dbg_log(
                      false, DbgLogInfo, DbgDiskLog, Module, Tag, Line,
                      {daemon_log, Format, Args}),
                    noreply;
                true ->
                    noreply
            end;
        {cast, {dbg_log, Pid, Module, Tag, Line, Term}} ->
            case lists:member(Pid, DisabledProcesses) of
                false ->
                    write_to_dbg_log(
                      TtyAvailable, DbgLogInfo, DbgDiskLog, Module, Tag, Line,
                      Term),
                    noreply;
                true ->
                    noreply
            end;
        config_updated ->
            {NewDaemonLogInfo, NewDbgLogInfo, NewErrorLogInfo} =
                ReadConfig(),
            NewDaemonDiskLog =
                reopen_log(TtyAvailable, NewDaemonLogInfo, DaemonDiskLog,
                           NewDaemonLogInfo, DaemonDiskLog,
                           #daemon_log_info.file),
            NewDbgDiskLog =
                reopen_log(TtyAvailable, NewDaemonLogInfo, DaemonDiskLog,
                           NewDbgLogInfo, DbgDiskLog, #dbg_log_info.file),
            true = save_enable_state(
                     NewDaemonLogInfo, NewDbgLogInfo, NewErrorLogInfo),
            {noreply, S#state{daemon_log_info = NewDaemonLogInfo,
                              daemon_disk_log = NewDaemonDiskLog,
                              dbg_log_info = NewDbgLogInfo,
                              dbg_disk_log = NewDbgDiskLog}};
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            error_logger:error_report(
              {?MODULE, ?LINE, {unknown_message, UnknownMessage}}),
            noreply
    end.

tty_available() ->
    case init:get_argument(detached) of
        {ok, [[]]} ->
            false;
        error ->
            true
    end.

%%
%% (Re)Open and close logs
%%

open_log(#daemon_log_info{enabled = true, file = {true, Path}}) ->
    disk_log:open([{name, daemon_log}, {file, ?b2l(Path)}, {format, external}]);
open_log(#dbg_log_info{enabled = true, file = {true, Path}}) ->
    disk_log:open([{name, dbg_log}, {file, ?b2l(Path)}, {format, external}]);
open_log(_LogInfo) ->
    {ok, undefined}.

reopen_log(TtyAvailable, DaemonLogInfo, DaemonDiskLog, LogInfo, DiskLog,
           FileField) ->
    close_log(DiskLog),
    case open_log(LogInfo) of
        {ok, NewDiskLog} when NewDiskLog /= undefined ->
            {true, Path} = element(FileField, LogInfo),
            write_to_daemon_log(TtyAvailable, DaemonLogInfo, DaemonDiskLog,
                                self(), ?MODULE, tag, "~s: reopened", [Path]),
            NewDiskLog;
        {ok, undefined} ->
            undefined;
        {error, DiskLogReason} ->
            error_logger:error_report({?MODULE, ?LINE, DiskLogReason}),
            undefined
    end.

close_log(undefined) -> ok;
close_log(Log) -> disk_log:close(Log).

save_enable_state(#daemon_log_info{enabled = DaemonLogEnabled},
                  #dbg_log_info{enabled = DbgLogEnabled},
                  #error_log_info{enabled = ErrorLogEnabled}) ->
    true = ets:insert(?MODULE, {daemon, DaemonLogEnabled}),
    true = ets:insert(?MODULE, {dbg, DbgLogEnabled}),
    ets:insert(?MODULE, {error, ErrorLogEnabled}).

%%
%% Daemon log
%%

write_to_daemon_log(true, #daemon_log_info{
                      enabled = true,
                      tty = Tty,
                      file = {FileEnabled, _Path},
                      show_filters = ShowFilters,
                      hide_filters = HideFilters},
                    DaemonDiskLog, _Pid, Module, Tag, Format, Args)
  when Tty == true; FileEnabled == true ->
    case show(Module, Tag, ShowFilters, HideFilters) of
        true ->
            String = io_lib:format("==== ~s ====\n" ++ Format,
                                   [format_date()|Args]),
            write_to_daemon_log(DaemonDiskLog, Tag, String),
            write_to_daemon_tty(Tty, Tag, String);
        false ->
            skip
    end;
write_to_daemon_log(_TtyAvailable, _DaemonLogInfo, _DaemonDiskLog, _Pid,
                    _Module, _Tag, _Format, _Args) ->
    skip.

write_to_daemon_log(undefined, _Tag, _String) -> ok;
write_to_daemon_log(Log, Tag, String) ->
    GregorianSeconds =
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    disk_log:balog(Log,
                   ["== (", ?a2l(Tag), ") ", ?i2l(GregorianSeconds), " ",
                    format_date(), $\n, String, $\n]).

write_to_daemon_tty(false, _Tag, _String) ->
    ok;
write_to_daemon_tty(true, Tag, String) ->
    io:format("~s", [["== DAEMON REPORT (", ?a2l(Tag), ") ", String, $\n]]).

%%
%% Debug log
%%

write_to_dbg_log(true, #dbg_log_info{enabled = true,
                                     tty = Tty,
                                     file = {FileEnabled, _Path},
                                     show_filters = ShowFilters,
                                     hide_filters = HideFilters},
                 DaemonDiskLog, Module, Tag, Line, Term)
  when Tty == true; FileEnabled == true ->
    case show(Module, Tag, ShowFilters, HideFilters) of
        true ->
            String = io_lib:format(
                       "==== ~s ===\n~w: ~w: ~w\n~p",
                       [format_date(), Module, Tag, Line, Term]),
            write_to_dbg_log(DaemonDiskLog, String),
            write_to_dbg_tty(Tty, String);
        false ->
            skip
    end;
write_to_dbg_log(_TtyAvailable, _DbgLogInfo, _DbgDiskLog, _Module, _Tag, _Line,
                 _Term) ->
    skip.

show(Module, Tag, ShowFilters, HideFilters) ->
    (is_member(Module, ShowFilters) orelse
     is_member(Tag, ShowFilters)) andalso
    (not(is_member(Module, HideFilters)) andalso
     not(is_member(Tag, HideFilters))).

is_member(_Tag, []) ->
    false;
is_member(_Tag, ['*'|_]) ->
    true;
is_member(TagList, [Tag|Rest]) when is_list(TagList) ->
    case is_member(Tag, TagList) of
        true ->
            true;
        false ->
            is_member(TagList, Rest)
    end;
is_member(Tag, [Tag|_]) ->
    true;
is_member(Tag, [_|Rest]) ->
    is_member(Tag, Rest).

write_to_dbg_log(undefined, _String) ->
    ok;
write_to_dbg_log(Log, String) ->
    disk_log:balog(Log, [String, $\n]).

write_to_dbg_tty(false, _String) ->
    ok;
write_to_dbg_tty(true, String) ->
    io:format("~s", [["== DEBUG REPORT ", String, $\n]]).

%%
%% Date formatting
%%

format_date() ->
    Now = erlang:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_local_time(Now),
    MilliSeconds = element(3, Now) div 1000,
    io_lib:format("~w-~s-~w::~2..0w:~2..0w:~2..0w.~3..0w",
		  [Day, month2string(Month), Year, Hour, Minute, Second,
                   MilliSeconds]).

month2string(1) -> "Jan";
month2string(2) -> "Feb";
month2string(3) -> "Mar";
month2string(4) -> "Apr";
month2string(5) -> "May";
month2string(6) -> "Jun";
month2string(7) -> "Jul";
month2string(8) -> "Aug";
month2string(9) -> "Sep";
month2string(10) -> "Oct";
month2string(11) -> "Nov";
month2string(12) -> "Dec".
