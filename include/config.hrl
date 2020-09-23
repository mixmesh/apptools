-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, true).

-define(config(JsonPath),
        config_serv:lookup(skernel_config_serv, JsonPath)).
-define(configd(JsonPath, DefaultJsonValue),
        config_serv:lookup(skernel_config_serv, JsonPath, DefaultJsonValue)).

-endif.
