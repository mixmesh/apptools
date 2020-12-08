-ifndef(CONFIG_SCHEMA_HRL).
-define(CONFIG_SCHEMA_HRL, true).

-record(json_type,
        {name :: config_serv:type_name(),
         info :: binary() | string() | undefined,
         typical :: config_serv:json_value(),
         transform :: fun((config_serv:json_value()) -> any()) |
                      undefined,
         untransform :: fun((any()) -> any()) |
                        undefined,
         reloadable = true :: boolean()}).

-endif.
