-module(serv).
-export([spawn_server/3, spawn_server/4]).
-export([cast/2]).
-export([call/2, call/3]).
-export([reply/2]).
-export([system_code_change/4,
         system_continue/3,
         system_get_state/1,
         system_replace_state/2,
         system_terminate/4,
         write_debug/3]).
-export_type([name/0, spawn_server_result/0]).

-include_lib("apptools/include/serv.hrl").

-type name() :: atom() | pid().
-type spawn_server_result() :: {ok, pid()} | {error, any()}.

%% Exported: spawn_server

-spec spawn_server(atom(), any(), fun(), #serv_options{}) ->
                          spawn_server_result().

spawn_server(ModuleName, InitState, MessageHandler) ->
    spawn_server(ModuleName, InitState, MessageHandler,
                 #serv_options{module_name = ModuleName}).

spawn_server(ModuleName, InitState, MessageHandler,
             #serv_options{timeout = Timeout, link = Link} = Options) ->
    Parent = self(),
    PerformInit =
        fun() ->
                true = put_options(Options, ModuleName, Parent, MessageHandler),
                init(InitState, MessageHandler, Options, Parent)
        end,
    Pid =
        if
            Link ->
                proc_lib:spawn_link(PerformInit);
            true ->
                proc_lib:spawn(PerformInit)
        end,
    receive
        {Pid, ok} ->
            {ok, Pid};
        {Pid, {error, Reason}} ->
            {error, Reason}
    after
        Timeout ->
            {error, timeout}
    end.

put_options(Options, ModuleName, Parent, MessageHandler) ->
    undefined == put(serv_options,
                     Options#serv_options{module_name = ModuleName,
                                          parent = Parent,
                                          message_handler = MessageHandler}).

get_options() ->
    get(serv_options).

init(InitState, MessageHandler,
     #serv_options{name = Name, trap_exit = TrapExit}, Parent) ->
    case register_server(Name) of
        true ->
            if
                TrapExit ->
                    process_flag(trap_exit, true);
                true ->
                    ignore
            end,
            if
                is_function(InitState) ->
                    case InitState(Parent) of
                        {ok, State} ->
                            Parent ! {self(), ok},
                            loop(MessageHandler, State);
                        {error, Reason} ->
                            Parent ! {self(), {error, Reason}}
                    end;
                true ->
                    Parent ! {self(), ok},
                    loop(MessageHandler, InitState)
            end;
        {error, Reason} ->
            Parent ! {self(), {error, Reason}}
    end.

register_server(none) ->
    true;
register_server(Name) ->
    case is_pid(whereis(Name)) of
        true ->
            {error, already_started};
        false ->
            register(Name, self())
    end.

loop(MessageHandler, State) ->
    case MessageHandler(State) of
        stop ->
            stopped;
        {stop, {Pid, Ref}, Reply} ->
            Pid ! {reply, Ref, Reply},
            stopped;
        {reply, {Pid, Ref}, Reply} ->
            Pid ! {reply, Ref, Reply},
            loop(MessageHandler, State);
        {reply, {Pid, Ref}, Reply, NewState} ->
            Pid ! {reply, Ref, Reply},
            loop(MessageHandler, NewState);
        noreply ->
            loop(MessageHandler, State);
        {noreply, NewState} ->
            loop(MessageHandler, NewState);
        {system, From, Request} ->
            #serv_options{module_name = ModuleName,
                          parent = Parent,
                          debug_options = DebugOptions} =
                get_options(),
            sys:handle_system_msg(Request, From, Parent, ModuleName,
                                  DebugOptions, State);
        {swap_message_handler, NewMessageHandler} ->
            loop(NewMessageHandler, State);
        {swap_message_handler, NewMessageHandler, NewState} ->
            loop(NewMessageHandler, NewState);
        UnknownMessage ->
            throw({unknown_message, UnknownMessage})
    end.

%% Exported: cast

-spec cast(serv:name(), any()) -> ok.

cast(To, Request) when is_pid(To) ->
    To ! {cast, Request},
    ok;
cast(To, Request) ->
    case whereis(To) of
        undefined ->
            io:format("BAD: ~p\n", [To]),




            throw(badarg);
        Pid ->
            Pid ! {cast, Request},
            ok
    end.

%% Exported: call

-spec call(serv:name(), any(), integer() | infinity) ->
                  any() | {error, timeout}.

call(To, Request) ->
  call(To, Request, infinity).

call(To, Request, Timeout) ->
    Pid =
        if
            is_pid(To) ->
                To;
            true ->
                case whereis(To) of
                    undefined ->
                        throw(badarg);
                    ToPid ->
                        ToPid
                end
        end,
    Ref = make_ref(),
    Pid ! {call, {self(), Ref}, Request},
    receive
        {reply, Ref, Reply} ->
            Reply
    after
        Timeout ->
            {error, timeout}
    end.

%% Exported: reply

-spec reply({pid(), reference()}, any()) -> ok.

reply({Pid, Ref}, Reply) ->
    Pid ! {reply, Ref, Reply},
    ok.

%% Exported: system_code_changed

system_code_change(State, ModuleName, OldVersion, Extra) ->
    case get_options() of
        #serv_options{system_code_change = not_set} ->
            {ok, State};
        #serv_options{system_code_change = SystemCodeChange} ->
            SystemCodeChange(State, ModuleName, OldVersion, Extra)
    end.

%% Exported: system_continue

system_continue(Parent, DebugOptions, State) ->
    case get_options() of
        #serv_options{message_handler = MessageHandler,
                      system_continue = not_set} ->
            loop(MessageHandler, State);
        #serv_options{message_handler = MessageHandler,
                      system_continue = SystemContinue} ->
            NewState = SystemContinue(Parent, DebugOptions, State),
            loop(MessageHandler, NewState)
    end.

%% Exported: system_get_state

system_get_state(State) ->
    case get_options() of
        #serv_options{system_get_state = not_set} ->
            {ok, State};
        #serv_options{system_get_state = SystemGetState} ->
            SystemGetState(State)
    end.

%% Exported: system_replace_state

system_replace_state(StateFun, State) ->
    case get_options() of
        #serv_options{system_replace_state = not_set} ->
            NewState = StateFun(State),
            {ok, NewState, NewState};
        #serv_options{system_replace_state = SystemReplaceState} ->
            SystemReplaceState(StateFun, State)
    end.

%% Exported: system_terminate

system_terminate(Reason, Parent, DebugOptions, State) ->
    case get_options() of
        #serv_options{system_terminate = not_set} ->
            exit(Reason);
        #serv_options{system_terminate = SystemTerminate} ->
            SystemTerminate(Reason, Parent, DebugOptions, State)
    end.

%% Exported: write_debug

write_debug(Device, Event, Name)  ->
    case get_options() of
        #serv_options{write_debug = not_set} ->
            io:format(Device, "~p event = ~p~n", [Name, Event]);
        #serv_options{write_debug = WriteDebug} ->
            WriteDebug(Device, Event, Name)
    end.
