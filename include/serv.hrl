-ifndef(SERV_HRL).
-define(SERV_HRL, true).

-record(serv_options, {module_name = not_set,
                       name = none,
                       parent = not_set,
                       timeout = infinity,
                       link = true,
                       trap_exit = true,
                       debug_options = [],
                       message_handler = not_set,
                       system_code_change = not_set,
                       system_continue = not_set,
                       system_get_state = not_set,
                       system_replace_state = not_set,
                       system_terminate = not_set,
                       write_debug = not_set}).

-define(spawn_server(InitState, MessageHandler, Options),
        serv:spawn_server(?MODULE, InitState, MessageHandler, Options)).

-endif.
