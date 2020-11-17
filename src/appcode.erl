-module(appcode).
-export([l/1, lm/0]).

%% Exported: l

l(Module) ->
    case c:l(Module) of
        {'module', Module} ->
            ok = trigger_serv_processes(Module, processes()),
            {'module', Module};
        OrElse ->
            OrElse
    end.

trigger_serv_processes(_Module, []) ->
    ok;
trigger_serv_processes(Module, [Pid|Rest]) ->
    case process_info(Pid, [current_function, dictionary]) of
        [{current_function, {Module, _FunctionName, _Arity}},
         {dictionary, DictionaryValues}] ->
            case lists:keysearch('$initial_call', 1, DictionaryValues) of
                {value, {_, {serv, init, 5}}} ->
                    Pid ! {system, undefined, code_switch},
                    trigger_serv_processes(Module, Rest);
                _ ->
                    trigger_serv_processes(Module, Rest)
            end;
        _ ->
            trigger_serv_processes(Module, Rest)
    end.

%% Exported: lm

lm() ->
    [l(Module) || Module <- code:modified_modules()].
