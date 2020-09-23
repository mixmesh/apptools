# Application tools

```
include/config.hrl
include/config_schema.hrl
src/config_serv.erl
```

An extendible config server which makes it possible for applications
to have data driven config files in JSON format.

```
-rw-rw-r-- 1 jocke jocke 700 sep 23 07:59 log.hrl
-rw-rw-r-- 1 jocke jocke 669 sep 23 07:59 stdio_log.hrl
-rw-rw-r-- 1 jocke jocke 750 sep 23 07:59 log_serv.hrl
-rw-rw-r-- 1 jocke jocke 12318 sep 23 22:14 log_serv.erl
```

An extendible log server which makes it possible for applications
to have create log messages in various ways.

```
-rw-rw-r-- 1 jocke jocke  6605 sep 23 22:22 serv.erl
-rw-rw-r-- 1 jocke jocke 830 sep 23 19:36 serv.hrl
```

A light-weight gen_server replacement which brings the fun back to
server programming.

```
-rw-rw-r-- 1 jocke jocke 631 sep 23 08:15 bits.hrl
```

Bit manipulation macros

```
-rw-rw-r-- 1 jocke jocke 657 sep 23 08:15 shorthand.hrl
```

Shorthand macros for long BIF functions names such as
list_to_integer/1.
