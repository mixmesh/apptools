-module(apptools_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]). %% Used by supervisor:start_link/2

%%
%% Exported: start_link
%%

-spec start_link() -> any().

start_link() ->
    case supervisor:start_link(?MODULE, []) of
        {ok, SupervisorPid} ->
            {ok, SupervisorPid};
        Error ->
            Error
    end.

%%
%% Exported: init
%%

init([]) ->
    ServManager =
	#{id => serv_manager,
          start => {serv_manager, start_link, []}},
    {ok, {#{strategy => one_for_all},
          [ServManager]}}.
