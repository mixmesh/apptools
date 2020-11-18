-ifndef(LOG_SERV_HRL).
-define(LOG_SERV_HRL, true).

-record(daemon_log_info,
        {enabled :: boolean(),
         tty :: boolean(),
         file :: {Enabled :: boolean(), Filename :: binary()},
         show_filters :: log_serv:filter_tags(),
         hide_filters :: log_serv:filter_tags()}).

-record(dbg_log_info,
        {enabled :: boolean(),
         tty :: boolean(),
         file :: {Enabled :: boolean(), Filename :: binary()},
         show_filters :: log_serv:filter_tags(),
         hide_filters :: log_serv:filter_tags()}).

-record(error_log_info,
        {enabled :: boolean(),
         tty :: boolean(),
         file :: {Enabled :: boolean(), Filename :: binary()}}).

-endif.
