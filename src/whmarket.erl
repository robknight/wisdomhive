-module(whmarket).

-include("whrecords.hrl").
-include("wh_commands.hrl").

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
       

%% UTILITY FUNCTIONS %%

create_tables() ->
  mnesia:create_table(market,  [{disc_copies, [node()]}, {attributes, record_info(fields, market)}]),
  mnesia:create_table(account, [{disc_copies, [node()]}, {attributes, record_info(fields, account)}]).


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

change_contract_quantity([], ContractName, Change) ->
  [].

get_portfolio_entry([PE|Portfolio], MarketName, ContractName) ->
  case { PE#portfolio_entry.market_name, PE#portfolio_entry.contract_name } of
    { MarketName, ContractName} ->
      PE;
    _ ->
      get_portfolio_entry(Portfolio, MarketName, ContractName)
  end;

get_portfolio_entry([], MarketName, ContractName) ->
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
cost_to_trade(ContractList, ContractName, Quantity) ->
  100 * (cost_at_quantity(ContractList, ContractName, Quantity) - cost_at_quantity(ContractList, ContractName, 0)).


% This function determines how many contracts need to be bought or sold in order
% to drive the price for the contract to Price
% The return value is positive if contracts should be bought, negative if they
% should be sold
% WARNING: return value will not be a whole integer and should be rounded
% accordingly by the calling function
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

% All commands are forwarded to the gen_server process for the market in
% question.

% At this stage, we can expect that all of the inputs will be well-formed
% records, with all of the required fields present.  However, they contents
% of the records may not be valid.  Here we are concerned only with invariants -
% making sure that the record does not violate some "eternal" rule.
%

execute(Command) when is_record(Command, buy) ->
  buy(Command);

execute(Command) when is_record(Command, sell) ->
  sell(Command);
  
execute(Command) when is_record(Command, create_contract) ->
  create_contract(Command);

execute(Command) when is_record(Command, create_market) ->
  create_market(Command);

execute(Command) when is_record(Command, create_account) ->
  create_account(Command);

execute(Command) when is_record(Command, open_market) ->
  open_market(Command);
  
execute(Command) when is_record(Command, close_market) ->
  close_market(Command);
  
execute(_Command) ->
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

create_contract(Command) when
  not is_list(Command#create_contract.market_name);
  not is_list(Command#create_contract.contract_name);
  not is_list(Command#create_contract.user);
  not is_list(Command#create_contract.description) ->
    { error, badly_formed };

create_contract(#create_contract{market_name = MarketName, contract_name = ContractName,
user = User, description = Description } = Command) ->
  F = fun() ->
    case get_market(MarketName) of
      { ok, Market } ->
        case Market#market.status of
          open ->
            mnesia:abort({ error, market_is_open });
          closed ->
            Contract = #contract{ name =  ContractName, description = Description },
            case add_contract(Market#market.contracts, Contract) of
              { ok, NewContractList } ->
                NewMarket = Market#market { contracts = NewContractList },
                mnesia:write(NewMarket);
              Error ->
                mnesia:abort(Error)
            end
        end;
      Error ->
        mnesia:abort(Error)
    end
  end,
  mnesia:transaction(F).


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
quantity = Quantity, max_price = MaxPrice } = Command) ->
  F = fun() ->
    Account = get_account(User),
    Market = get_market(MarketName),
    ContractList = Market#market.contracts,
    case get_contract(Market#market.contracts, ContractName) of
      false ->
        mnesia:abort(contract_does_not_exist);
      Contract when MaxPrice =< Contract#contract.price ->
        mnesia:abort(max_price_too_low);
      Contract ->
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
            mnesia:write(NewMarket)
        end
    end
  end,
  mnesia:transaction(F).
  
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
quantity = Quantity, min_price = MinPrice } = Command) ->
  F = fun() ->
    Account = get_account(User),
    Market = get_market(MarketName),
    ContractList = Market#market.contracts,
    case get_portfolio_entry(Account#account.portfolio, MarketName, ContractName) of
      false ->
        mnesia:abort(no_contracts_to_sell);
      PortfolioEntry when PortfolioEntry#portfolio_entry.quantity == 0 ->
        mnesia:abort(no_contracts_to_sell);
      PortfolioEntry ->
        case get_contract(Market#market.contracts, ContractName) of
          false ->
            mnesia:abort(contract_does_not_exist);
          Contract when MinPrice >= Contract#contract.price ->
            mnesia:abort(min_price_too_high);
          Contract ->
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
                mnesia:write(NewMarket)
            end
        end
    end
  end,
  mnesia:transaction(F).

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

create_market(Command) when
  not is_list(Command#create_market.market_name);
  not is_list(Command#create_market.user);
  not is_list(Command#create_market.description) ->
    { error, badly_formed };

create_market(#create_market{market_name = MarketName, user = User } = Command) ->
  F = fun() ->
    case get_market(MarketName) of
      { error, market_not_found } ->
        Market = #market{ name = MarketName, created_date = erlang:now(), creator = User },
        mnesia:write(Market);
      _ ->
        mnesia:abort(market_already_exists)
    end
  end,
  mnesia:transaction(F).
  
% Create Account Command
% Parameters:
% user          JID of the account owner
%               Must be a string (list)

create_account(Command) when
  not is_list(Command#create_account.user) ->
  { error, badly_formed };

create_account(#create_account{ user = Owner } = Command) ->
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
  mnesia:transaction(F).
  
% Open Market Command
% Parameters
% market_name         Name of the market
%                     Must be a string (list)
% user                JID of the user
%                     Must be a string (list)

open_market(Command) when
  not is_list(Command#open_market.user);
  not is_list(Command#open_market.market_name) ->
    { error, badly_formed };

open_market(#open_market{ market_name = MarketName, user = User }) ->
  change_status(MarketName, User, open).

change_status(MarketName, User, NewStatus) ->
  F = fun() ->
    case get_market(MarketName) of
      { error, Reason } ->
        mnesia:abort(Reason);
      { ok, Market } when Market#market.creator =/= User ->
        mnesia:abort({ error, no_permission });
      { ok, Market } when Market#market.status =:= NewStatus ->
        mnesia:abort({ error, no_change });
      { ok, Market } ->
        NewMarket = Market#market{ status = NewStatus },
        mnesia:write(NewMarket)
    end 
  end,
  mnesia:transaction(F).
  
% Close Market Command
% Parameters
% market_name         Name of the market
%                     Must be a string(list)
% user                JID of the user
%                     Must be a string(list)

close_market(Command) when
  not is_list(Command#close_market.market_name);
  not is_list(Command#close_market.user) ->
    { error, badly_formed };

close_market(#close_market{ market_name = MarketName, user = User }) ->
  change_status(MarketName, User, closed).