-module(wh_bot_events).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init([Session]) ->
  {ok, 500}.

handle_event(Event#event {sequence = Seq, source = MarketName,
  #trade_event{ contract_name = ContractName, quantity = Quantity}, Session) ->
  
  {ok, Session};

handle_event(Event, State) ->
  io:format('~nEvent: ~p~n', [Event]),
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