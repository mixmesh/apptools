-module(log_serv).
-export([start_link/1, is_log_enabled/1]).
-export([daemon_log/6, dbg_log/5]).
-export([add_daemon_show/1, remove_daemon_show/1]).
-export([add_daemon_hide/1, remove_daemon_hide/1]).
-export([add_dbg_show/1, remove_dbg_show/1]).
-export([add_dbg_hide/1, remove_dbg_hide/1]).
-export([format_error/1]).
-export([message_handler/1]).

-export_type([filter_tags/0]).
-export_type([error_reason/0]).

-include("../include/log_serv.hrl").
-include("../include/shorthand.hrl").
-include("../include/serv.hrl").

-record(state,
        {parent :: pid(),
         tty_available :: boolean(),
         read_config :: read_config(),
         daemon_log_info :: #daemon_log_info{} | undefined,
         daemon_disk_log :: any(),
         dbg_log_info :: #dbg_log_info{} | undefined,
         dbg_disk_log :: any(),
         error_log_info :: #error_log_info{} | undefined}).

-type tag() :: atom().
-type tags() :: [tag()].
-type tag_or_tags() :: tag() | tags().
-type filter_tag() :: '*' | atom() | {proc, pid() | atom()}.
-type filter_tags() :: [filter_tag()].
-type filter_tag_or_tags() :: filter_tag() | filter_tags().
-type read_config() ::
        fun(() -> {#daemon_log_info{}, #dbg_log_info{}, #error_log_info{}}).
-type error_reason() :: already_started | term().

%%
%% Exported: start_link
%%

-spec start_link(read_config()) ->
          serv:spawn_server_result() | {error, error_reason()}.

start_link(ReadConfig) ->
    ?spawn_server_opts(
       fun(Parent) -> init(Parent, ReadConfig, tty_available()) end,
       fun ?MODULE:message_handler/1,
       #serv_options{name = ?MODULE}).

%%
%% Exported: is_log_enabled
%%

-spec is_log_enabled(daemon | dbg | error) -> boolean().

is_log_enabled(LogType) ->
    try ets:lookup(?MODULE, LogType) of
	[{LogType, Enabled}] ->
	    Enabled
    catch
	error:badarg ->
	    false
    end.

%%
%% Exported: daemon_log
%%

-spec daemon_log(pid(), atom(), tag_or_tags(), integer(), string(),
                 [any()]) ->
          ok.

daemon_log(Pid, Module, TagOrTags, Line, Format, Args) ->
    serv:cast(?MODULE, {daemon_log, Pid, Module, TagOrTags, Line, Format,
                        Args}).

%%
%% Exported: dbg_log
%%

-spec dbg_log(pid(), atom(), tag_or_tags(), Line :: integer(), term()) -> ok.

dbg_log(Pid, Module, TagOrTags, Line, Term) ->
    serv:cast(?MODULE, {dbg_log, Pid, Module, TagOrTags, Line, Term}).

%%
%% Exported: add_daemon_show
%%

-spec add_daemon_show(filter_tag_or_tags()) -> filter_tags().

add_daemon_show(FilterTags) ->
    update_filter(add, daemon, show, FilterTags).

update_filter(Op, LogType, ShowOrHide, FilterTags) when is_list(FilterTags) ->
    serv:call(
      ?MODULE,
      {update_filter, Op, LogType, ShowOrHide, lookup_pids(FilterTags)});
update_filter(Op, LogType, ShowOrHide, FilterTag) ->
    serv:call(
      ?MODULE,
      {update_filter, Op, LogType, ShowOrHide, [lookup_pid(FilterTag)]}).

%%
%% Exported: remove_daemon_show
%%

-spec remove_daemon_show(filter_tag_or_tags()) -> filter_tags().

remove_daemon_show(FilterTags) ->
    update_filter(remove, daemon, show, FilterTags).

%%
%% Exported: add_daemon_hide
%%

-spec add_daemon_hide(filter_tag_or_tags()) -> filter_tags().

add_daemon_hide(FilterTags) ->
    update_filter(add, daemon, hide, FilterTags).

%%
%% Exported: remove_daemon_hide
%%

-spec remove_daemon_hide(filter_tag_or_tags()) -> filter_tags().

remove_daemon_hide(FilterTags) ->
    update_filter(remove, daemon, hide, FilterTags).

%%
%% Exported: add_dbg_show
%%

-spec add_dbg_show(filter_tag_or_tags()) -> filter_tags().

add_dbg_show(FilterTags) ->
    update_filter(add, dbg, show, FilterTags).

%%
%% Exported: remove_dbg_show
%%

-spec remove_dbg_show(filter_tag_or_tags()) -> filter_tags().

remove_dbg_show(FilterTags) ->
    update_filter(remove, dbg, show, FilterTags).

%%
%% Exported: add_dbg_hide
%%

-spec add_dbg_hide(filter_tag_or_tags()) -> filter_tags().

add_dbg_hide(FilterTags) ->
    update_filter(add, dbg, hide, FilterTags).

%%
%% Exported: remove_dbg_hide
%%

-spec remove_dbg_hide(filter_tag_or_tags()) -> filter_tags().

remove_dbg_hide(FilterTags) ->
    update_filter(remove, dbg, hide, FilterTags).

%%
%% Exported: format_error
%%

-spec format_error(error_reason()) -> iolist().

format_error(already_started) ->
    "Already started";
format_error(Reason) ->
    disk_log:format_error(Reason).

%%
%% Server
%%

init(Parent, ReadConfig, TtyAvailable) ->
    {DaemonLogInfo, DbgLogInfo, ErrorLogInfo} = ReadConfig(),
    open_log_1(DaemonLogInfo, DbgLogInfo, ErrorLogInfo,
	       #state{parent = Parent,
		      tty_available = TtyAvailable,
		      read_config = ReadConfig}).

open_log_1(DaemonLogInfo, DbgLogInfo, ErrorLogInfo, S) ->
    case open_log(DaemonLogInfo) of
        {ok, DaemonDiskLog} ->
	    open_log_2(DbgLogInfo, ErrorLogInfo,
		       S#state {
			 daemon_log_info = DaemonLogInfo,
			 daemon_disk_log = DaemonDiskLog});
	{error,Reason} ->
            {error, Reason}
    end.

open_log_2(DbgLogInfo, ErrorLogInfo, S) ->
    case open_log(DbgLogInfo) of
        {ok, DbgDiskLog} ->
	    open_log_3(ErrorLogInfo,
		       S#state {
			 dbg_log_info = DbgLogInfo,
			 dbg_disk_log = DbgDiskLog});
	{error,Reason} ->
            {error, Reason}
    end.

open_log_3(ErrorLogInfo, S) ->
    case ErrorLogInfo of
	#error_log_info{enabled = true, file = {true, Filename}} ->
	    case error_logger:add_report_handler(
		   log_mf_h,
		   log_mf_h:init(?b2l(Filename),
				 1024*1024*1024, 2,
				 fun({error, _, _}) ->
					 true;
				    ({error_report, _, _}) ->
					 true;
				    (_) ->
					 false
				 end)) of
		ok ->
		    open_log_final(S#state { error_log_info = ErrorLogInfo });

		{error,Error} ->
		    %% FIXME: fail?
		    {error, Error}
	    end;
	#error_log_info{enabled = true, file = {false, _Filename}} ->
	    open_log_final(S#state{error_log_info = ErrorLogInfo});
	#error_log_info{enabled = false } ->
	    open_log_final(S#state { error_log_info = ErrorLogInfo })
    end.

open_log_final(S) ->
    case whereis(config_server) of
        undefined ->
            skip;
        _ ->
            ok = config_serv:subscribe()
    end,
    ?MODULE = ets:new(?MODULE, [public, named_table]),
    true = save_enable_state(
	     S#state.daemon_log_info,
	     S#state.dbg_log_info,
	     S#state.error_log_info),
    {ok, S}.

message_handler(#state{parent = Parent,
                       tty_available = TtyAvailable,
                       read_config = ReadConfig,
                       daemon_log_info = DaemonLogInfo,
                       daemon_disk_log = DaemonDiskLog,
                       dbg_log_info = DbgLogInfo,
                       dbg_disk_log = DbgDiskLog,
                       error_log_info = _ErrorLogInfo} = S) ->
    receive
        {cast, {daemon_log, Pid, Module, TagOrTags, Line, Format, Args}} ->
            write_to_daemon_log(
              TtyAvailable, DaemonLogInfo, DaemonDiskLog, Pid, Module,
              TagOrTags, Format, Args),
            write_to_dbg_log(
              false, DbgLogInfo, DbgDiskLog, Pid, Module, TagOrTags, Line,
              {daemon_log, Format, Args}),
            noreply;
        {cast, {dbg_log, Pid, Module, TagOrTags, Line, Term}} ->
            write_to_dbg_log(
              TtyAvailable, DbgLogInfo, DbgDiskLog, Pid, Module, TagOrTags,
              Line, Term),
            noreply;
        {call, From, {update_filter, Op, LogType, ShowOrHide, FilterTags}} ->
            Filters =
                case {LogType, ShowOrHide} of
                    {daemon, show} ->
                        DaemonLogInfo#daemon_log_info.show_filters;
                    {dbg, show} ->
                        DbgLogInfo#dbg_log_info.show_filters;
                    {daemon, hide} ->
                        DaemonLogInfo#daemon_log_info.hide_filters;
                    {dbg, hide} ->
                        DbgLogInfo#dbg_log_info.hide_filters
                end,
            UpdatedFilters =
                lists:foldl(
                  fun(FilterTag, Acc) ->
                          case Op of
                              add ->
                                  case lists:member(FilterTag, Acc) of
                                      true ->
                                          Acc;
                                      false ->
                                          [FilterTag|Acc]
                                  end;
                              remove ->
                                  lists:delete(FilterTag, Acc)
                          end
                  end, Filters, FilterTags),
            NewState =
                case {LogType, ShowOrHide} of
                    {daemon, show} ->
                        S#state{daemon_log_info =
                                    DaemonLogInfo#daemon_log_info{
                                      show_filters = UpdatedFilters}};
                    {dbg, show} ->
                        S#state{dbg_log_info =
                                    DbgLogInfo#dbg_log_info{
                                      show_filters = UpdatedFilters}};
                    {daemon, hide} ->
                        S#state{daemon_log_info =
                                    DaemonLogInfo#daemon_log_info{
                                      hide_filters = UpdatedFilters}};
                    {dbg, hide} ->
                        S#state{dbg_log_info =
                                    DbgLogInfo#dbg_log_info{
                                      hide_filters = UpdatedFilters}}
                end,
            {reply, From, UpdatedFilters, NewState};
        config_updated ->
            {NewDaemonLogInfo, NewDbgLogInfo, NewErrorLogInfo} = ReadConfig(),
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
                    DaemonDiskLog, Pid, Module, TagOrTags, Format, Args)
  when Tty == true; FileEnabled == true ->
    case show(Pid, Module, TagOrTags, ShowFilters, HideFilters) of
        true ->
            String = io_lib:format("==== ~s ====\n" ++ Format,
                                   [format_date()|Args]),
            write_to_daemon_log(DaemonDiskLog, TagOrTags, String),
            write_to_daemon_tty(Tty, TagOrTags, String);
        false ->
            skip
    end;
write_to_daemon_log(_TtyAvailable, _DaemonLogInfo, _DaemonDiskLog, _Pid,
                    _Module, _TagOrTags, _Format, _Args) ->
    skip.

write_to_daemon_log(undefined, _TagOrTags, _String) -> ok;
write_to_daemon_log(Log, TagOrTags, String) ->
    GregorianSeconds =
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    disk_log:balog(
      Log,
      ["== (", format_tag_or_tags(TagOrTags), ") ", ?i2l(GregorianSeconds),
       " ", format_date(), $\n, String, $\n]).

write_to_daemon_tty(false, _TagOrTags, _String) ->
    ok;
write_to_daemon_tty(true, TagOrTags, String) ->
    io:format("~s", [["== DAEMON REPORT (", format_tag_or_tags(TagOrTags),
                      ") ", String, $\n]]).

%%
%% Debug log
%%

write_to_dbg_log(true, #dbg_log_info{enabled = true,
                                     tty = Tty,
                                     file = {FileEnabled, _Path},
                                     show_filters = ShowFilters,
                                     hide_filters = HideFilters},
                 DaemonDiskLog, Pid, Module, TagOrTags, Line, Term)
  when Tty == true; FileEnabled == true ->
    case show(Pid, Module, TagOrTags, ShowFilters, HideFilters) of
        true ->
            String = io_lib:format(
                       "==== ~s ===\n~w: ~s: ~w\n~p",
                       [format_date(), Module, format_tag_or_tags(TagOrTags),
                        Line, Term]),
            write_to_dbg_log(DaemonDiskLog, String),
	    TtyString =
		try iolist_to_binary(Term) of
		    Bin ->
			io_lib:format(
			  "==== ~s ===\n~w: ~s: ~w\n~s",
			  [format_date(), Module, format_tag_or_tags(TagOrTags),
                           Line, Bin])
		catch
		    error:_ ->
			String
		end,
            write_to_dbg_tty(Tty, TtyString);
        false ->
            skip
    end;
write_to_dbg_log(_TtyAvailable, _DbgLogInfo, _DbgDiskLog, _Pid, _Module,
                 _TagOrTags, _Line, _Term) ->
    skip.

%%
%% Filtering
%%

lookup_pid({proc, Name}) when is_atom(Name) ->
    {proc, whereis(Name)};
lookup_pid(FilterTag) ->
    FilterTag.

lookup_pids([]) ->
    [];
lookup_pids([FilterTag|Rest]) ->
    [lookup_pid(FilterTag)|lookup_pids(Rest)].

show(Pid, Module, TagOrTags, ShowFilters, HideFilters) ->
    tags_member(Pid, Module, TagOrTags, ShowFilters) andalso
	not tags_member(Pid, Module, TagOrTags, HideFilters).

tags_member(Pid, Module, TagOrTags, Filter) ->
    is_member(Pid, Module, Filter) orelse is_member(Pid, TagOrTags, Filter).

is_member(_Pid, _TagOrTags, []) ->
    false;
is_member(_Pid, _TagOrTags, ['*'|_]) ->
    true;
is_member(Pid, _TagOrTags, [{proc, Pid}|_]) ->
    true;
is_member(Pid, TagOrTags, [{proc, _AnotherPid}|Rest]) ->
    is_member(Pid, TagOrTags, Rest);
is_member(Pid, TagOrTags, [Tag|Rest]) when is_list(TagOrTags) ->
    case is_member(Pid, Tag, TagOrTags) of
        true ->
            true;
        false ->
            is_member(Pid, TagOrTags, Rest)
    end;
is_member(_Pid, Tag, [Tag|_]) ->
    true;
is_member(Pid, Tag, [_|Rest]) ->
    is_member(Pid, Tag, Rest).

write_to_dbg_log(undefined, _String) ->
    ok;
write_to_dbg_log(Log, String) ->
    disk_log:balog(Log, [String, $\n]).

write_to_dbg_tty(false, _String) ->
    ok;
write_to_dbg_tty(true, String) ->
    io:format("~s", [["== DEBUG REPORT ", String, $\n]]).

%%
%% Formatting
%%

format_date() ->
    Now = erlang:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_local_time(Now),
    MilliSeconds = element(3, Now) div 1000,
    io_lib:format("~w-~s-~w::~2..0w:~2..0w:~2..0w.~3..0w",
		  [Day, month2string(Month), Year, Hour, Minute, Second,
                   MilliSeconds]).

format_tag_or_tags(TagOrTags) ->
    io_lib:format("~p", [TagOrTags]).

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
