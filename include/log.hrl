-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-define(error_log(_Term),
	error_logger:error_report({?MODULE, ?LINE, (_Term)})).
-define(daemon_log(_Format, _Args),
        log_serv:daemon_log(obscrete_log_serv, self(), ?MODULE, tag, ?LINE,
                            _Format, _Args)).
-define(daemon_tag_log(_Tag, _Format, _Args),
        log_serv:daemon_log(obscrete_log_serv, self(), ?MODULE, _Tag, ?LINE,
                            _Format, _Args)).
-define(dbg_log(_Term),
        log_serv:dbg_log(obscrete_log_serv, self(), ?MODULE, tag, ?LINE,
                         (_Term))).
-define(dbg_tag_log(_Tag, _Term),
        log_serv:dbg_log(obscrete_log_serv, self(), _Tag, ?LINE, (_Term))).

-endif.
