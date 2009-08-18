-module(whlogger).

-include("whrecords.hrl").
-include("wh_commands.hrl").

-include_lib("stdlib/include/qlc.hrl").

-behaviour(gen_event).

%% TODO: write this

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-export([create_tables/0]).

-record(wh_event, {timestamp, id, details}).

create_tables() ->
  mnesia:create_table(events, [{disc_copies, [node()]}, {attributes, record_info(fields, wh_event)}]).

init([]) ->
  {ok, []}.

handle_event(_Event, State) ->
  {ok, State}.
  
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_events(Tab, Pattern) ->
  