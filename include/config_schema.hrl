-ifndef(CONFIG_SCHEMA_HRL).
-define(CONFIG_SCHEMA_HRL, true).

-record(json_type, {
          name :: config_serv:type_name(),
          info :: binary() | string() | undefined,
          typical :: config_serv:json_value(),
          convert :: fun((config_serv:json_value()) ->
                                config_serv:json_value()) |
                        undefined,
          reloadable = true :: boolean()
         }).

-endif.
