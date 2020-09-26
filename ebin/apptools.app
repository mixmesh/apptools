%% -*- erlang -*-
{application, apptools,
 [{description,"Application tools"},
  {vsn, "1.0"},
  {modules, [config,
             config_serv,
             log_serv,
             serv,
             unit_test_engine]},
  {registered, [obscrete_app, obscrete_sup]},
  {env, []},
  {applications, [kernel, stdlib]}]}.
