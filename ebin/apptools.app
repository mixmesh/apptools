%% -*- erlang -*-
{application, apptools,
 [{description,"Application tools"},
  {vsn, "0.9.1"},
  {modules, [config,
             config_serv,
             log_serv,
             serv,
             test_engine]},
  {applications, [kernel, stdlib]}]}.
