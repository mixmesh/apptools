# Application tools

A set of services and libraries to make it easier to write applications and daemons in Erlang

## Files

<dl>
  <dt>./src/config_serv.erl</dt>
  <dd>An extendible config server which makes it possible for applications to have data driven config files in JSON format</dd>
  <dt>./src/log_serv.erl</dt>
  <dd>An extendible log server which makes it possible for applications to create/filter log messages in various ways</dd>
  <dt>./src/test_engine.erl</dt>
  <dd>Used by the ./mixmesh/bin/run_test command to make it easy to start a test</dd>
  <dt>./src/serv.erl</dt>
  <dd>A light-weight gen_server replacement which brings the fun back to server programming</dd>
  <dt>./include/bits.hrl</dt>
  <dd>Your favourite bit manipulation macros</dd>
  <dt>./include/shorthand.hrl</dd>
  <dd>A number of BIFs have fairly long names. A bunch of shorthand macros comes to the rescue.</dd>
</dl>
