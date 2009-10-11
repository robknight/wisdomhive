-module(whmarket).

-include("../include/whrecords.hrl").
-include("../include/wh_commands.hrl").
-include("../include/wh_events.hrl").

-compile(export_all).

-define(MSR_B, 100).
-define(MAX_PRICE, 100).
 
-export([
         create_contract/1,
         buy/1,
         sell/1,
         create_market/1,
         create_account/1,
         open_market/1,
         close_market/1
       ]).
       
-export([create_tables/0]).
       

start() ->
  wh_event_manager:start_link().

%% UTILITY FUNCTIONS %%


floor(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end.

ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.


create_tables() ->
  mnesia:create_table(market,  [{disc_copies, [node()]}, {attributes, record_info(fields, market)}]),
  mnesia:create_table(account, [{disc_copies, [node()]}, {attributes, record_info(fields, account)}]),
  mnesia:create_table(event,  [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, event)}]).

mnesia_error_or(F, Or) ->
  case mnesia:transaction(F) of
    { aborted, { error, Error }} ->
      { error, Error };
    { aborted, AbortReason } ->
      { aborted, AbortReason }; % might throwing an exception make sense here?
    { atomic, _Val } ->
      Or
  end.
  
mnesia_error_or_ok(F) ->
  mnesia_error_or(F, ok).

get_all_markets() ->
  F = fun() ->
    Pattern = #market{_ = '_'},
    mnesia:match_object(Pattern)
  end,
  case mnesia:transaction(F) of
    { atomic, Val } ->
      Val;
    { aborted, Reason } ->
      { error, Reason}
  end.

get_market(Name) ->
  F = fun() ->
    case mnesia:read({ market, Name }) of
      [] ->
        mnesia:abort(market_not_found);
      [ Market|_T ] ->
        Market
    end
  end,
  
  Result = mnesia:transaction(F),
  case Result of
    { aborted, Reason } ->
      { error, Reason };
    { atomic, Val } ->
      Val
  end.

get_account(Owner) ->
  F = fun() ->
    case mnesia:read({ account, Owner }) of
      [] ->
        mnesia:abort(account_not_found);
      [ Account|_T ] ->
        Account
    end
  end,
  
  Result = mnesia:transaction(F),
  case Result of
    { aborted, Reason } ->
      { error, Reason };
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

get_contract([C|T], Name) ->
  case C#contract.name of
    Name ->
      C;
    _ ->
      get_contract(T, Name)
  end;

get_contract([], _Name) ->
  false.

add_contract(List, #contract{ name = Name } = Contract) ->
  case contract_exists(List, Name) of
    true ->
      { error, contract_already_exists };
    false ->
      { ok, update_prices([Contract|List]) }
  end.

change_contract_quantity([C|ContractList], ContractName, Change) ->
  case C#contract.name of
    ContractName ->
      NewC = C#contract{ quantity = C#contract.quantity + Change },
      [NewC|ContractList];
    _ ->
      [C|change_contract_quantity(ContractList, ContractName, Change)]
  end;

change_contract_quantity([], _ContractName, _Change) ->
  [].

get_portfolio_entry([PE|Portfolio], MarketName, ContractName) ->
  case { PE#portfolio_entry.market_name, PE#portfolio_entry.contract_name } of
    { MarketName, ContractName} ->
      PE;
    _ ->
      get_portfolio_entry(Portfolio, MarketName, ContractName)
  end;

get_portfolio_entry([], _MarketName, _ContractName) ->
  [].

%% TRADING  %%

add_to_portfolio(MarketName, ContractName , Quantity, [PE|Portfolio]) ->
  { portfolio_entry, M, C, Qty } = PE,
  case { M, C } of
    { MarketName, ContractName } ->
      NewPE = PE#portfolio_entry{ quantity = Qty + Quantity },
      [NewPE|Portfolio];
    _ ->
      [PE|add_to_portfolio(MarketName, ContractName, Quantity, Portfolio)]
  end;

add_to_portfolio(MarketName, ContractName, Quantity, []) ->
  NewPE = #portfolio_entry{ market_name = MarketName, contract_name = ContractName, quantity = Quantity },
  [NewPE].


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

-spec sum_powers(list()) -> any().
sum_powers([C|T]) ->
  Quantity = C#contract.quantity,
  math:exp(Quantity/?MSR_B) + sum_powers(T);

sum_powers([]) ->
  0.

-spec cost_at_quantity(list(), binary(), any()) -> any().
cost_at_quantity(ContractList, ContractName, Quantity) ->
  F = fun(C) ->
    case C#contract.name of
      ContractName ->
        C#contract{ quantity=C#contract.quantity + Quantity };
      _ ->
        C
    end
  end,
  NewList = lists:map(F, ContractList),
  S = sum_powers(NewList),
  math:log(S) * ?MSR_B.

% This function calculates the cost of altering the quantity of a contract by
% Quantity (positive for buying, negative for selling)
-spec cost_to_trade(list(), binary(), any()) -> any().
cost_to_trade(ContractList, ContractName, Quantity) ->
  100 * (cost_at_quantity(ContractList, ContractName, Quantity) - cost_at_quantity(ContractList, ContractName, 0)).


% This function determines how many contracts need to be bought or sold in order
% to drive the price for the contract to Price
% The return value is positive if contracts should be bought, negative if they
% should be sold
% WARNING: return value will not be a whole integer and should be rounded
% accordingly by the calling function
-spec quantity_for_price(list(), binary(), any()) -> any().
quantity_for_price(ContractList, ContractName, Price) ->
  Contract = get_contract(ContractList, ContractName),
  Prob = Price / 100,
  F = fun(C) ->
    case C#contract.name of
      ContractName ->
        0;
      _ ->
        Prob * math:exp(C#contract.quantity / ?MSR_B)
      end
  end,
  D = lists:sum(lists:map(F, ContractList)),
  Res = math:log((D/(1-Prob))) * ?MSR_B,
  Res - Contract#contract.quantity.

% This function determines how many contracts can be bought for the specified
% sum.
-spec quantity_for_sum(list(), binary(), any()) -> any().
quantity_for_sum(ContractList, ContractName, Sum) ->
  OldCost = cost_at_quantity(ContractList, ContractName, 0),
  N = math:exp((OldCost + (Sum / 100)) / ?MSR_B),
  Contract = get_contract(ContractList, ContractName),
  F = fun(C) ->
    case C#contract.name of
      ContractName ->
        0;
      _ ->
        math:exp(C#contract.quantity / ?MSR_B)
    end
  end,
  Qty = ?MSR_B * math:log(N - lists:sum(lists:map(F, ContractList))),
  Qty - Contract#contract.quantity.
  


%% Public Interface %%

%% COMMANDS %%

% There is a slight problem here.  What happens at present is this:
% All commands are received via execute/1.  If the command succeeds, the
% command is broadcast as an event which, amongst other things, will be logged.
% However, two problems exist:
%
% 1) The resulting log of commands might not accurately reflect the order in
% which the events were processed, due to race conditions.  Perhaps the event
% could be raised from within the mnesia transaction?
% 2) We really want to record the *outcome* of the command rather than [just]
% the command itself.  Full details of the command are probably necessary to
% give context to the event, but when we're broadcasting to somewhat dumb event
% recipients, we just want to tell them 'X happened', not 'here are the details
% of what we just did, you figure out what happened as a result'.  Perhaps each
% command should return an event record?  For example, a 'buy' command might not
% specifies maximum prices and maximum numbers of contracts to buy, but those
% not always going to be the actual price paid or quantity bought - we want to
% record those as well.
% POSSIBLE SOLUTION: write to an event log from within the mnesia transaction,
% and after each command either read from the log or notify another process to
% do so.  The event log will always be in the correct order.

-spec event(wh_event(), any()) -> ok.
event(Event, Source) ->
  wh_event_manager:log_event(Event, Source).

execute(Command) ->
  Result = dispatch(Command),
  case Result of
    { error, _ } ->
      Result;
    { aborted, _ } ->
      Result;
    _ ->
      wh_event_manager:update(),
      Result
  end.

-spec dispatch(wh_command()) -> { error, any()} | { aborted, any() } | ok.
dispatch(Command=#buy{}) ->
  buy(Command);

dispatch(Command=#sell{}) ->
  sell(Command);
  
dispatch(Command=#create_contract{}) ->
  create_contract(Command);

dispatch(Command=#create_market{}) ->
  create_market(Command);

dispatch(Command=#create_account{}) ->
  create_account(Command);

dispatch(Command=#open_market{}) ->
  open_market(Command);
  
dispatch(Command=#close_market{}) ->
  close_market(Command);
  
dispatch(_Command) ->
  throw(unknown_command).


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
-spec create_contract(#create_contract{}) -> { error, any()} | { aborted, any() } | ok.
create_contract(Command) when
  not is_list(Command#create_contract.market_name);
  not is_list(Command#create_contract.contract_name);
  not is_list(Command#create_contract.user);
  not is_list(Command#create_contract.description) ->
    { error, badly_formed };

create_contract(#create_contract{market_name = MarketName, contract_name = ContractName,
 description = Description } = _Command) ->
  F = fun() ->
    case get_market(MarketName) of
      { error, Error } ->
        mnesia:abort(Error);
      Market ->
        case Market#market.status of
          open ->
            mnesia:abort({ error, market_is_open });
          closed ->
            Contract = #contract{ name =  ContractName, description = Description },
            case add_contract(Market#market.contracts, Contract) of
              { ok, NewContractList } ->
                NewMarket = Market#market { contracts = NewContractList },
                mnesia:write(NewMarket),
                event(#create_contract_event{ name = ContractName, description = Description }, { market, MarketName });
              Error ->
                mnesia:abort(Error)
            end
        end
    end
  end,
  mnesia_error_or_ok(F).


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

-spec buy(#buy{}) -> { error, any()} | { aborted, any() } | ok.
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

buy(#buy{max_price = MaxPrice}) when MaxPrice >= ?MAX_PRICE ->
  { error, max_price_too_high };

buy(#buy{market_name = MarketName, user = User, contract_name = ContractName,
quantity = Quantity, max_price = MaxPrice }) ->
  F = fun() ->
    Account = get_account(User),
    Market = get_market(MarketName),
    ContractList = Market#market.contracts,
    case get_contract(Market#market.contracts, ContractName) of
      false ->
        mnesia:abort(contract_does_not_exist);
      Contract when MaxPrice =< Contract#contract.price ->
        mnesia:abort(max_price_too_low);
      _ ->
        Balance = Account#account.balance,
        AffordableMax = quantity_to_buy(ContractList, ContractName, Quantity, MaxPrice, Balance),
        if
          AffordableMax < 1 ->
            mnesia:abort(insufficient_funds);
          true ->
            Cost = cost_to_trade(ContractList, ContractName, AffordableMax),
            NewPortfolio = add_to_portfolio(MarketName, ContractName, AffordableMax, Account#account.portfolio),
            NewAccount = Account#account{ balance = Balance - Cost, portfolio = NewPortfolio },
            NewContractList = update_prices(change_contract_quantity(Market#market.contracts, ContractName, AffordableMax)),
            NewMarket = Market#market{ contracts = NewContractList },
            mnesia:write(NewAccount),
            mnesia:write(NewMarket),
            event(#trade_event{quantity = AffordableMax, contract_name = ContractName}, { market, MarketName }),
            event(#balance_change_event{amount = -Cost}, {user, User})
        end
    end
  end,
  mnesia_error_or_ok(F).

-spec quantity_to_buy(list(), binary(), any(), any(), any()) -> any().
quantity_to_buy(ContractList, ContractName, MaxQuantity, MaxPrice, Balance) ->
  QFP = floor(quantity_for_price(ContractList, ContractName, MaxPrice)),
  RealMax = if
    QFP > MaxQuantity ->
      MaxQuantity;
    true ->
      QFP
  end,
  Cost = cost_to_trade(ContractList, ContractName, RealMax),
  if
    Cost > Balance ->
      floor(quantity_for_sum(ContractList, ContractName, Balance));
    true ->
      RealMax
  end.
  
% Sell Command
% All details identical to the Buy command, except:
% min_price instead of max_price
-spec sell(#sell{}) -> { error, any()} | { aborted, any() } | ok.
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

% Possible TODOs:
% What happens if Market doesn't exist?
% Consider re-working using exceptions
% Since everything occurs within a Mnesia transaction, it's possible that instead
% of checking return values (with the nested case statements) we could just throw
% exceptions from get_market(), get_account() etc.
% In practice these exceptions *should* be rare.
sell(#sell{market_name = MarketName, user = User, contract_name = ContractName,
quantity = Quantity, min_price = MinPrice } = _Command) ->
  F = fun() ->
    Account = get_account(User),
    Market = get_market(MarketName),
    ContractList = Market#market.contracts,
    case get_portfolio_entry(Account#account.portfolio, MarketName, ContractName) of
      [] ->
        mnesia:abort(no_contracts_to_sell);
      PortfolioEntry when PortfolioEntry#portfolio_entry.quantity == 0 ->
        mnesia:abort(no_contracts_to_sell);
      PortfolioEntry ->
        case get_contract(Market#market.contracts, ContractName) of
          false ->
            mnesia:abort(contract_does_not_exist);
          Contract when MinPrice >= Contract#contract.price ->
            mnesia:abort(min_price_too_high);
          _ ->
            Balance = Account#account.balance,
            SaleQuantity = 0 - quantity_to_sell(ContractList, ContractName, Quantity, MinPrice, PortfolioEntry),
            if
              SaleQuantity > -1 ->
                mnesia:abort(demand_fail);  % possibly need a better error name
              true ->
                Revenue = 0 - cost_to_trade(ContractList, ContractName, SaleQuantity),
                NewPortfolio = add_to_portfolio(MarketName, ContractName, SaleQuantity, Account#account.portfolio),
                NewAccount = Account#account{ balance = Balance + Revenue, portfolio = NewPortfolio },
                NewContractList = update_prices(change_contract_quantity(Market#market.contracts, ContractName, SaleQuantity)),
                NewMarket = Market#market{ contracts = NewContractList },
                mnesia:write(NewAccount),
                mnesia:write(NewMarket),
                event(#trade_event{quantity = SaleQuantity, contract_name = ContractName}, { market, MarketName }),
                event(#balance_change_event{amount = Revenue}, {user, User})
            end
        end
    end
  end,
  mnesia_error_or_ok(F).

-spec quantity_to_sell(list(), binary(), any(), any(), #portfolio_entry{}) -> any().
quantity_to_sell(ContractList, ContractName, MaxQuantity, MinPrice, PortfolioEntry) ->
  QtyToSell = if
    MaxQuantity > PortfolioEntry#portfolio_entry.quantity ->
      PortfolioEntry#portfolio_entry.quantity;
    true ->
      MaxQuantity
  end,
  QtyForPrice = 0 - quantity_for_price(ContractList, ContractName, MinPrice),
  if
    QtyToSell > QtyForPrice ->
      QtyForPrice;
    true ->
      QtyToSell
  end.

% Create Market Command
% Parameters:
% market_name         Name of the newly-created market
%                     Must be a string (list)
% user                JID of the user
%                     Must be a string (list)
% description         Description of the market
%                     Must be a string (list)

-spec create_market(#create_market{}) -> { error, any()} | { aborted, any() } | ok.
create_market(Command) when
  not is_list(Command#create_market.market_name);
  not is_list(Command#create_market.user);
  not is_list(Command#create_market.description) ->
    { error, badly_formed };

create_market(#create_market{market_name = MarketName, user = User } = _Command) ->
  F = fun() ->
    case get_market(MarketName) of
      { error, market_not_found } ->
        Market = #market{ name = MarketName, created_date = erlang:now(), creator = User },
        mnesia:write(Market),
        event(#create_market_event{ user = User }, { market, MarketName });
      _ ->
        mnesia:abort(market_already_exists)
    end
  end,
  mnesia_error_or_ok(F).
  
% Create Account Command
% Parameters:
% user          JID of the account owner
%               Must be a string (list)

-spec create_account(#create_account{}) -> { error, any()} | { aborted, any() } | ok.
create_account(Command) when
  not is_list(Command#create_account.user) ->
  { error, badly_formed };

create_account(#create_account{ user = Owner } = _Command) ->
  F = fun() ->
    case get_account(Owner) of
      { error, account_not_found } ->
        Account = #account{ owner = Owner, balance = 10000, created_date = erlang:now(), portfolio = []},
        mnesia:write(Account);
      { error, Reason } ->
        mnesia:abort(Reason);
      _ ->
        mnesia:abort(account_already_exists)
    end
  end,
  mnesia_error_or_ok(F).
  
% Open Market Command
% Parameters
% market_name         Name of the market
%                     Must be a string (list)
% user                JID of the user
%                     Must be a string (list)

-spec open_market(#open_market{}) -> { error, any()} | { aborted, any() } | ok.
open_market(Command) when
  not is_list(Command#open_market.user);
  not is_list(Command#open_market.market_name) ->
    { error, badly_formed };

open_market(#open_market{ market_name = MarketName, user = User }) ->
  change_status(MarketName, User, open).

-spec change_status(binary(), binary(), atom()) -> { error, any()} | { aborted, any() } | ok.
change_status(MarketName, User, NewStatus) ->
  F = fun() ->
    case get_market(MarketName) of
      { error, Reason } ->
        mnesia:abort(Reason);
      Market when Market#market.creator =/= User ->
        mnesia:abort({ error, no_permission });
      Market when Market#market.status =:= NewStatus ->
        mnesia:abort({ error, no_change });
      Market ->
        NewMarket = Market#market{ status = NewStatus },
        mnesia:write(NewMarket),
        event(#market_status_event{ status = NewStatus }, {market, MarketName})
    end 
  end,
  mnesia_error_or_ok(F).
  
% Close Market Command
% Parameters
% market_name         Name of the market
%                     Must be a string(list)
% user                JID of the user
%                     Must be a string(list)

-spec close_market(#close_market{}) -> { error, any()} | { aborted, any() } | ok.
close_market(Command) when
  not is_list(Command#close_market.market_name);
  not is_list(Command#close_market.user) ->
    { error, badly_formed };

close_market(#close_market{ market_name = MarketName, user = User }) ->
  change_status(MarketName, User, closed).