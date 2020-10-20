-ifndef(LOG_HRL).
-define(LOG_HRL, true).

%% Daemon log macros

-define(daemon_log_fmt(_Format, _Args),
        case log_serv:is_log_enabled(daemon) of
            true ->
                log_serv:daemon_log(self(), ?MODULE, tag, ?LINE, (_Format),
                                    (_Args));
            false ->
                silence
        end).

-define(daemon_log_tag_fmt(_Tag, _Format, _Args),
        case log_serv:is_log_enabled(daemon) of
            true ->
                log_serv:daemon_log(self(), ?MODULE, (_Tag), ?LINE, (_Format),
                                    (_Args));
            false ->
                silence
        end).

%% Debug log macros

-define(dbg_log(_Term),
        case log_serv:is_log_enabled(dbg) of
            true ->
                log_serv:dbg_log(self(), ?MODULE, tag, ?LINE, (_Term));
            false ->
                silence
        end).

-define(dbg_log_tag(_Tag, _Term),
        case log_serv:is_log_enabled(dbg) of
            true ->
                log_serv:dbg_log(self(), ?MODULE, (_Tag), ?LINE, (_Term));
            false ->
                silence
        end).

-define(dbg_log_fmt(_Format, _Args),
        case log_serv:is_log_enabled(dbg) of
            true ->
                log_serv:dbg_log(
                  self(), ?MODULE, tag, ?LINE,
                  lists:flatten(io_lib:format((_Format), (_Args))));
            false ->
                silence
        end).

-define(dbg_log_tag_fmt(_Tag, _Format, _Args),
        case log_serv:is_log_enabled(dbg) of
            true ->
                log_serv:dbg_log(
                  self(), ?MODULE, (_Tag), ?LINE,
                  lists:flatten(io_lib:format((_Format), (_Args))));
            false ->
                silence
        end).

%% Error log macros

-define(error_log(_Term),
        case log_serv:is_log_enabled(error) of
            true ->
                error_logger:error_report({?MODULE, ?LINE, (_Term)});
            false ->
                silence
        end).

-define(error_log_fmt(_Format, _Args),
        case log_serv:is_log_enabled(error) of
            true ->
                error_logger:error_report(
                  {?MODULE, ?LINE,
                   lists:flatten(io_lib:format((_Format), (_Args)))});
            false  ->
                silence
        end).

-endif.
