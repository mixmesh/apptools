-ifndef(SERV_HRL).
-define(SERV_HRL, true).

-record(serv_options, {module_name = not_set          :: atom(),
                       name = none                    :: serv:name(),
                       parent = not_set               :: pid() | not_set,
                       timeout = infinity             :: integer() | infinity,
                       link = true                    :: boolean(),
                       trap_exit = true               :: boolean(),
                       debug_options = []             :: [sys:debug_option()],
                       message_handler = not_set      :: not_set | fun(),
                       system_code_change = not_set   :: not_set | fun(),
                       system_continue = not_set      :: not_set | fun(),
                       system_get_state = not_set     :: not_set | fun(),
                       system_replace_state = not_set :: not_set | fun(),
                       system_terminate = not_set     :: not_set | fun(),
                       write_debug = not_set          :: not_set | fun()}).

-define(spawn_server(InitState, MessageHandler),
        serv:spawn_server(?MODULE, InitState, MessageHandler)).

-define(spawn_server_opts(InitState, MessageHandler, Options),
        serv:spawn_server(?MODULE, InitState, MessageHandler, Options)).

-endif.
