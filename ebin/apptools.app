%% -*- erlang -*-
{application, apptools,
 [{description,"Application tools"},
  {vsn, "1.0"},
  {modules, [config,
             config_serv,
             log_serv,
             serv,
             unit_test_engine]},
  {applications, [kernel, stdlib]}]}.
