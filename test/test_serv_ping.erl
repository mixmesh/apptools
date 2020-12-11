-module(test_serv_ping).
-export([start_link/0, ping/1]).
-export([init/1, message_handler/1]).

-include_lib("apptools/include/serv.hrl").
-include_lib("apptools/include/log.hrl").

start_link() ->
    ?spawn_server({?MODULE, init, []}, {?MODULE, message_handler}).

ping(Pid) ->
    serv:call(Pid, ping).

init(Parent) ->
    {ok, Parent}.

message_handler(Parent) ->
    receive
        {call, From, ping} ->
            {reply, From, pong};
        {system, From, Request} ->
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?error_log({usjnknjown_message, UnknownMessage}),
            noreply
    end.
