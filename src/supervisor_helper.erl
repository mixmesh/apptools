-module(supervisor_helper).
-export([foreach_worker/2, get_selected_worker_pids/2]).

%% Exported: foreach_worker

foreach_worker(SupervisorPid, Do) ->
    Workers =
        lists:foldl(fun({Id, Pid, worker, _}, Acc) ->
                            [{Id, Pid}|Acc];
                       (_, Acc) ->
                            Acc
                    end, [], supervisor:which_children(SupervisorPid)),
    foreach_worker(Do, Workers, Workers).

foreach_worker(_Do, _Workers, []) ->
    ok;
foreach_worker(Do, Workers, [{Id, Pid}|Rest]) ->
    Do(Id, Pid, Workers),
    foreach_worker(Do, Workers, Rest).

%% Exported: get_selected_worker_pids

get_selected_worker_pids([], _NeighbourWorkers) ->
    [];
get_selected_worker_pids([Id|Rest], NeighbourWorkers) ->
    case lists:keytake(Id, 1, NeighbourWorkers) of
        {value, {Id, Pid}, RemainingNeighbourWorkers} ->
            [Pid|get_selected_worker_pids(Rest, RemainingNeighbourWorkers)];
        false ->
            [undefined|get_selected_worker_pids(Rest, NeighbourWorkers)]
    end.
