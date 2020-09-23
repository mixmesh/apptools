-ifndef(LOG_HRL).
-define(LOG_HRL, true).

-define(error_log(_Term),
	io:format("~p~n", [{error_log, ?MODULE, ?LINE, (_Term)}])).
-define(daemon_log(_Format, _Args),
        io:format("~p~n",
                  [{daemon_log, self(), ?MODULE, ?LINE, _Format, _Args}])).
-define(daemon_tag_log(_Tag, _Format, _Args),
        io:format("~p~n",
                  [{daemon_tag_log, self(), _Tag, ?LINE, _Format, _Args}])).
-define(dbg_log(_Term),
        io:format("~p~n",
                  [{dbg_log, self(), ?MODULE, ?LINE, (_Term)}])).
-define(dbg_tag_log(_Tag, _Term),
        io:format("~p~n",
                  [{dbg_tag_log, self(), _Tag, ?LINE, (_Term)}])).

-endif.
