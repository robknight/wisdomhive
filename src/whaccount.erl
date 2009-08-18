-module(whaccount).

-behaviour(gen_server).

-include("whrecords.hrl").
-include("wh_commands.hrl").

%-compile(export_all).

-export([start_link/1]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

raise_event(EventManager, Event) ->
  gen_event:notify(EventManager, Event).
  
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init(Args) ->
    State = ok,
    { ok, State }.

handle_call(_Request, _From, State) ->
    io:write("Fallback Handler"),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
   % ejabberd_router:unregister_route(Host),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
