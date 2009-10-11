-module(wh).

-export([start/0, start/2, stop/1, setup/0]).

-spec start() -> ok.
start() ->
	ok = exmpp:start(),
  ok = mnesia:start(),
	application:start(wh_app).

-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  wh_supervisor:start_link().	

-spec stop(any()) -> any().
stop(_State) ->
	ok.

-spec setup() -> any().
setup() ->
  whmarket:create_tables().