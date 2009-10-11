-module(wh_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link(?MODULE, []).

-spec init(any()) -> {ok, {tuple(), [tuple()]}}.
init([]) ->
	BotSpec = {whbot, 
    { whbot, start_link, [] },
			permanent, 1000, worker, [whbot]},
      
  EventMgrSpec = {wh_event_manager,
    { wh_event_manager, start_link, [] },
    permanent, 1000, worker, dynamic
  },

	{ok, {{one_for_one,1, 10}, [EventMgrSpec, BotSpec]}}.
