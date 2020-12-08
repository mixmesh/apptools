-module(test_serv).
-export([start/0]).

start() ->
    spawn(fun() ->
                  {ok, Pid} = test_serv_ping:start_link(),
                  io:format("1"),
                  pong = test_serv_ping:ping(Pid),
                  io:format("2"),
                  {module, test_serv_ping} = purge_and_load(test_serv_ping),
                  io:format("3"),
                  pong = test_serv_ping:ping(Pid),
                  io:format("4"),
                  {module, test_serv_ping} = purge_and_load(test_serv_ping),
                  io:format("5"),
                  pong = test_serv_ping:ping(Pid),
                  io:format("6"),
                  {module, test_serv_ping} = purge_and_load(test_serv_ping),
                  io:format("7"),
                  pong = test_serv_ping:ping(Pid),
                  io:format("8"),
                  timer:sleep(500000)
          end).

purge_and_load(Module) ->
    LoadedFilename = code:which(Module),
    _ = code:purge(Module),
    {ok, LoadedBinary} = file:read_file(LoadedFilename),
    code:load_binary(Module, LoadedFilename, LoadedBinary).
