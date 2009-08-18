-module(whmarket).

-behaviour(gen_server).

-include("whrecords.hrl").
-include("wh_commands.hrl").

%-compile(export_all).

-define(MSR_B, 100).
-define(MAX_PRICE, 100).

-record(market_proc_state,
  {
  state,
  tab
  }
).

-record(market_state,
  {
  contracts,
  em
  }
).

-export([start_link/1]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).
 
-export([
         create_contract/1,
         buy/1,
         sell/1,
         create_market/1
       ]).
       
  
raise_event(EventManager, Event) ->
  gen_event:notify(EventManager, Event).
  
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init(Args) ->
    [Name|_T] = Args,
    EventManager = gen_event:start_link(),
    %State = #market_state{ name = Name, state = pending, em = EventManager },
    Tab = ets:new(?MODULE, [set, private]),
    ets:insert(Tab, { state, #market_state{ em = EventManager, contracts = [] }}),
    {ok, #market_proc_state{ state = pending, tab = Tab }}.

handle_call(#create_contract{ contract_name = _ContractName } = _Command, _From,
  #market_proc_state{ state = State } = MarketState) when State /= active, State /= created ->
  { reply, { error, market_in_invalid_state }, MarketState };
    
handle_call(#create_contract{ contract_name = ContractName } = Command, _From, State) ->
  #market_state{ contracts = Contracts } = Market = market(State#market_proc_state.tab),
  case contract_exists(Contracts, ContractName) of
    true ->
      { reply, { error, contract_already_exists }, State };
    false ->
      NewState = do_create_contract(Command, Market, State),
      { reply, ok, NewState }
  end;

handle_call(#create_market{ market_name = _MarketName } = _Command, _From,
  #market_proc_state{ state = State } = MarketState)
  when State /= pending ->
  { reply, { error, market_already_created }, MarketState };

handle_call(#create_market{ market_name = _MarketName } = Command, _From, MarketState) ->
  Market = market(MarketState#market_proc_state.tab),
  NewState = do_create_market(Command, Market, MarketState),
  {reply, ok, NewState };

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
    


%% UTILITY FUNCTIONS %%
market(Tab) ->
  ets:lookup(Tab, state).
  
save(Tab, State) ->
  ets:insert(Tab, { state, State }),
  ok.

get_market(Name) ->
  F = fun() ->
    case mnesia:read({ market, Name }) of
      [] ->
        { error, market_not_found };
      [ Market|_T ] ->
        { ok, Market }
    end
  end,
  
  Result = mnesia:transaction(F),
  case Result of
    { aborted, _ } ->
      { error, aborted };
    { atomic, Val } ->
      Val
  end.
  
contract_exists([C|T], Name) ->
  CName = C#contract.name,
  case CName of
    Name ->
      true;
    _ ->
      contract_exists(T, Name)
  end;
  
contract_exists([], _Name) ->
  false.



%% FUNCTIONS THAT ACTUALLY DO STUFF %%

do_create_market(Command, Market, State) ->
  raise_event(Market#market_state.em, Command),
  State#market_proc_state{ state = created }.

do_create_contract(Command, Market, State) ->
  #create_contract{ contract_name = Name, description = Description } = Command,
  NewContract = #contract{ name = Name, description = Description },
  ContractList = State#market_state.contracts,
  NewContractList = update_prices([NewContract|ContractList]),
  NewState = Market#market_state{ contracts = NewContractList },
  save(State#market_proc_state.tab, NewState),
  raise_event(Market#market_state.em, Command),
  %State#market_state{ contracts = NewContractList }.
  State.
  
%% PRICE CALCULATION %%

% possible optimisation here
% we could cache the result of sum_powers (for all contracts in market) in the database
% when buying a contract, calculate sum_powers for that individual contract
% subtract that from the cached value, and add on the new value (with quantity +/- N)
% this would save having to loop through the list twice when updating prices
% (once for the sum_powers() calculation and again to update list)
update_prices(List) ->
  S = sum_powers(List),
  update_prices(List, S).

update_prices([C|T], SumPowers) ->
  Quantity = C#contract.quantity,
  NewPrice = 100 * math:exp(Quantity/?MSR_B) / SumPowers,
  NewC = C#contract{ price=NewPrice },
  [NewC|update_prices(T, SumPowers)];

update_prices([], _) ->
  [].

sum_powers([C|T]) ->
  Quantity = C#contract.quantity,
  math:exp(Quantity/?MSR_B) + sum_powers(T);

sum_powers([]) ->
  0.

%% Public Interface %%

%% COMMANDS %%

% All commands are forwarded to the gen_server process for the market in
% question.

% At this stage, we can expect that all of the inputs will be well-formed
% records, with all of the required fields present.  However, they contents
% of the records may not be valid.  Here we are concerned only with invariants -
% making sure that the record does not violate some "eternal" rule.
%
% Context-sensitive rules will be applied by the market gen_server process at
% the last possible moment.

% Create Contract Command
% Parameters:
% market_name     The name of the market
%                 Must be a string (list)
% contract_name   Name of the contract to create
%                 Must be a string (list)
% user            JID of the user
%                 Must be a string (list)
% description     Brief description of the contract
%                 Must be a string (list)

create_contract(Command) when
  not is_list(Command#create_contract.market_name);
  not is_list(Command#create_contract.contract_name);
  not is_list(Command#create_contract.user);
  not is_list(Command#create_contract.description) ->
    { error, badly_formed };

create_contract(#create_contract{market_name = MarketName} = Command) ->
  MarketAtom = list_to_atom(MarketName),
  gen_server:call(MarketAtom, Command).


% Buy Command
% Parameters:
% quantity        The maximum quantity of a contract to buy, subject to funds
%                 being available.
%                 Must be > 0.
% max_price       The highest price the buyer is willing to pay
%                 Must be > 0 and < ?MAX_PRICE
% market_name     Name of the market (and thus of the gen_server process)
%                 Must be a string (list)
% contract_name   Name of the contract to buy
%                 Must be a string (list)
% user            JID of the user
%                 Must be a string (list)

% The Market Process will do further checking, e.g. to ensure that the named
% contract actually exists

buy(Command) when
  not is_list(Command#buy.contract_name);
  not is_list(Command#buy.market_name);
  not is_number(Command#buy.quantity);
  not is_number(Command#buy.max_price);
  not is_list(Command#buy.user) ->
    { error, badly_formed };

buy(#buy{quantity = Quantity}) when Quantity == 0 ->
  { error, quantity_is_zero };

buy(#buy{max_price = MaxPrice}) when MaxPrice == 0 ->
  { error, max_price_is_zero };

buy(#buy{max_price = MaxPrice}) when MaxPrice > ?MAX_PRICE ->
  { error, max_price_too_high };

buy(#buy{market_name = MarketName} = Command) ->
  gen_server:call(MarketName, Command).
  
% Sell Command
% All details identical to the Buy command, except:
% min_price instead of max_price

% TODO: consider renaming sell/buy to sell_order/buy_order or something similar
% to reflect asynchronous nature

sell(Command) when
  not is_list(Command#sell.contract_name);
  not is_list(Command#sell.market_name);
  not is_number(Command#sell.quantity);
  not is_number(Command#sell.min_price);
  not is_list(Command#sell.user) ->
    { error, badly_formed };

sell(#sell{quantity = Quantity}) when Quantity == 0 ->
  { error, quantity_is_zero };

sell(#sell{min_price = MinPrice}) when MinPrice == 0 ->
  { error, min_price_is_zero };

sell(#sell{min_price = MinPrice}) when MinPrice > ?MAX_PRICE ->
  { error, min_price_too_high };

sell(#sell{market_name = MarketName} = Command) ->
  gen_server:call(MarketName, Command).
  
% Create Market Command
% Parameters:
% market_name         Name of the newly-created market
%                     Must be a string (list)
% user                JID of the user
%                     Must be a string (list)
% description         Description of the market
%                     Must be a string (list)

create_market(Command) when
  not is_list(Command#create_market.market_name);
  not is_list(Command#create_market.user);
  not is_list(Command#create_market.description) ->
    { error, badly_formed };

create_market(#create_market{market_name = MarketName} = Command) ->
  %case get_market(MarketName) of
   % { error, market_not_found } ->
      MarketAtom = list_to_atom(MarketName),
      start_link(MarketAtom),
      gen_server:call(MarketAtom, Command).
    %{ ok, _ } ->
    %  { error, market_exists };
    %{ error, aborted } ->
    %  { error, database_error }
 % end.
