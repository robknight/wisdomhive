-module(whstorage).

-include("whrecords.hrl").
-include("wh_commands.hrl").

-behaviour(gen_event).

%% TODO: write this

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-export([create_tables/0]).

-export([get_market/1, get_account/1]).

create_tables() ->
  mnesia:create_table(market, [{ram_copies, [node()]}, {attributes, record_info(fields, market)}]),
  mnesia:create_table(account, [{ram_copies, [node()]}, {attributes, record_info(fields, account)}]).

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
  
market_names() ->
  mnesia:dirty_all_keys(market).
  
account_names() ->
  mnesia:dirty_all_keys(account).

get_market(Name) ->
  case mnesia:dirty_read({ market, Name }) of
    [] ->
      { error, market_not_found };
    [ Market|_T ] ->
      { ok, Market }
  end.

get_account(Name) ->
  case mnesia:dirty_read({ account, Name }) of
    [] ->
      { error, account_not_found };
    [ Account|_T ] ->
      { ok, Account }
  end.