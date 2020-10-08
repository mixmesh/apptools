-ifndef(SERV_HRL).
-define(SERV_HRL, true).

%% -type from() :: {pid(), reference()}.
%% -type message_handler() ::
%%          fun(({cast, any()}) ->
%%                     noreply |
%%                     {noreply | NewState :: any()} |
%%                     stop | 
%%                     {swap_message_handler, message_handler()} |
%%                     {swap_message_handler, message_handler(),
%%                      NewState :: any()} ;
%%              ({call, from(), any()}) ->
%%                     noreply |
%%                     {noreply, NewState :: any()} |
%%                     {reply, from(), Reply :: any()} |
%%                     {reply, from(), Reply :: any(), NewState :: any()} |
%%                     {stop, from(), any()} ;
%%              ({system, {pid(), Tag :: term()}, any()}) ->
%%                     {system, {pid(), Tag :: term()}, any()}).

-type message_handler() :: fun().

-record(serv_options,
        {module_name = not_set          :: atom(),
         name = none                    :: serv:name(),
         parent = not_set               :: pid() | not_set,
         timeout = infinity             :: integer() | infinity,
         link = true                    :: boolean(),
         trap_exit = true               :: boolean(),
         debug_options = []             :: [sys:debug_option()],
         message_handler = not_set      :: not_set | message_handler(),
         system_code_change = not_set   :: not_set | function(),
         system_continue = not_set      :: not_set | function(),
         system_get_state = not_set     :: not_set | function(),
         system_replace_state = not_set :: not_set | function(),
         system_terminate = not_set     :: not_set | function(),
         write_debug = not_set          :: not_set | fun()}).

-define(spawn_server(InitState, MessageHandler),
        serv:spawn_server(?MODULE, InitState, MessageHandler)).

-define(spawn_server_opts(InitState, MessageHandler, Options),
        serv:spawn_server(?MODULE, InitState, MessageHandler, Options)).

-endif.
