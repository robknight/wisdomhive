-module(wh_event_manager).

-behaviour(gen_server).

-include("../include/wh_events.hrl").

-compile(export_all).

-export([start_link/0, add_handler/1, notify/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, stop/0]).

-define(SERVER, ?MODULE).
-define(EVT_MGR, wh_event_manager_proc).

-record(state, { counter = 0 }).

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
	gen_server:call(?MODULE, stop).

-spec init([]) -> {ok, #state{}}.
init([]) ->
  gen_event:start_link({local, ?EVT_MGR}),
	{ok, #state{}}.

-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}} | {noreply, #state{}}.
handle_call(_Msg, _From, State) ->
	{reply, unexpected, State}.
  
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Msg, State) ->
	{noreply, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(update, State) ->
  Unprocessed = #event{ status = new, _ = '_' },
  F = fun() ->
    mnesia:match_object(Unprocessed)
  end,
  { atomic, Events } = mnesia:transaction(F),
  ProcessedEvents = process_events(Events),
  F2 = fun() ->
    lists:foreach(fun(E) ->
      mnesia:write(E)
    end, ProcessedEvents)
  end,
  mnesia:transaction(F2),
  {noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

-spec terminate(any(), #state{}) -> any().
terminate(_Reason, _State) ->
	ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
  
-spec process_events([#event{}]) -> [#event{}].
process_events([Event|T]) ->
  notify(Event),
  [Event#event{status = processed}|process_events(T)];

process_events([]) ->
  [].

-spec add_handler(atom()) -> ok | {'EXIT', term()} | term().
add_handler(Module) ->
  gen_event:add_handler(?EVT_MGR, Module, []).

-spec notify(wh_event()) -> ok.
notify(Event) ->
  gen_event:notify(?EVT_MGR, Event).
  

update() ->
  gen_server:cast(?MODULE, update).

-spec log_event(wh_event(), { atom(), any() }) -> ok.
log_event(Event, Source) ->
  F = fun() ->
    Last = mnesia:last(event),
    E = #event{ sequence = Last + 1, status = new, event = Event, source = Source },
    mnesia:write(E)
  end,
  mnesia:transaction(F).