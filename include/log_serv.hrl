-ifndef(LOG_SERV_HRL).
-define(LOG_SERV_HRL, true).

-record(daemon_log_info, {
          enabled :: boolean(),
          tty :: boolean(),
          file :: {Enabled :: boolean(), Filename :: binary()},
          show_filters :: [atom()],
          hide_filters :: [atom()]
        }).

-record(dbg_log_info, {
          enabled :: boolean(),
          tty :: boolean(),
          file :: {Enabled :: boolean(), Filename :: binary()},
          show_filters :: [atom()],
          hide_filters :: [atom()]
         }).

-record(error_log_info, {
          enabled :: boolean(),
          tty :: boolean(),
          file :: {Enabled :: boolean(), Filename :: binary()}
        }).

-endif.
