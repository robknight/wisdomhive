-module(whbot).
-behaviour(gen_mod).
-behaviour(gen_server).


% TODO
%
% Refactor as follows:
% Any command issued to the system must be validated and transformed into some
% kind of record/DTO.  If the command cannot be validated, a response is sent
% back to the client immediately.  If the command is validated, a response will
% be sent asynchronously when the command is processed.
%
% Queries do not necessarily require the same level of validation; it is
% acceptable for most queries to return something equivalent to "does not exist"
% in cases where the query parameters do not refer to a valid object.
%
% Basically this means adopting the "DDDD" concepts from Greg Young
% We need a "commands" repository, basically a list of orders to trade,
% which should be stored in the most fault-tolerant DB we have (SQL of some
% kind), while the current snapshot state of the system can be stored in Mnesia.
% We can periodically store snapshots in My/PgSQL too, and rebuild the Mnesia
% DB from most recent snapshot + commands if this proves necessary at any
% point.
%
% The key point is to ensure that any commands issued are validated and
% turned into standard command objects as early as possible, whether the
% commands are being issued via REST or via XMPP or whatever.

% Domain Driven Design
%
% Without going overboard in adopting DDD, the concept of the 'aggregate root'
% is useful here.  ARs provide useful boundaries, collecting related objects
% together.
%
% At this stage, I suspect that a Market should be an aggregate root, with
% contracts as entities within the Market.  The reason for this is that all of
% the entities owned by the Market are related to each other and make no sense
% outside of the context of the Market - this is because our market-maker will
% adjust prices of contracts within a market depending on trades made on other
% contracts within the market.  If we did not have a market maker then I would
% probably suggest that Contracts should be the aggregate roots instead.
%
% What this means is that all trades *within a single market* are serialized.
% Inputs are processed and turned into Command messages which are processed
% serially by the process responsible for controlling a single market. Obviously
% this does present a bottleneck in processing, but this can be mitigated by
% doing as much pre-processing of Command messages as possible in parallel.


-include("/home/robknight/ejabberd-2.0.5/src/ejabberd.hrl").
-include("/home/robknight/ejabberd-2.0.5/src/jlib.hrl").
-include_lib("stdlib/include/qlc.hrl").
%-include("rfc4627/include/rfc4627.hrl").
-include("whrecords.hrl").

-compile(export_all).

-define(CT, {"Content-Type", "text/html; charset=utf-8"}).
-define(HEADER, [?CT]).
-define(MSR_B, 100).

-export([start_link/2]).
-export([start/2,
         stop/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         process/2]).

-export([route/3]).
-export([create_tables/0]).

-define(PROCNAME, ejabberd_mod_whbot).
-define(BOTNAME, whbot).


%% -record(portfolio, { account_owner, contracts_owned }).

%% gen_mod & gen_server callbacks %%

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).
    
%create_tables() ->
 %   mnesia:create_table(account,   [{ram_copies, [node()]}, {attributes, record_info(fields, account)}]),
 %   mnesia:create_table(market,    [{ram_copies, [node()]}, {attributes, record_info(fields, market)}]).
  %  mnesia:create_table(log,       [{disc_copies, [node()]}, {attributes, record_info(fields, log)}]).

start(Host, Opts) ->
    create_tables(),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,
        {?MODULE, start_link, [Host, Opts]},
        temporary,
        1000,
        worker,
        [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

init([Host, Opts]) ->
    ?DEBUG("WHBOT: Starting whbot", []),
    % add a new virtual host / subdomain "trader".example.com
    MyHost = gen_mod:get_opt_host(Host, Opts, "trader.@HOST@"),
    ejabberd_router:register_route(MyHost, {apply, ?MODULE, route}),
    {ok, Host}.

handle_call(stop, _From, Host) ->
    {stop, normal, ok, Host}.

handle_cast(_Msg, Host) ->
    {noreply, Host}.

handle_info(_Msg, Host) ->
    {noreply, Host}.

terminate(_Reason, Host) ->
    ejabberd_router:unregister_route(Host),
    ok.

code_change(_OldVsn, Host, _Extra) ->
    {ok, Host}.
    
%% some utility functions for xmpp %%
    
strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.

send_presence(From, To, "") ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});

send_presence(From, To, TypeStr) ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [{"type", TypeStr}], []}).

echo(From, To, Body) ->
    send_message(From, To, "chat", Body).

send_message(From, To, TypeStr, BodyStr) ->
    XmlBody = {xmlelement, "message",
           [{"type", TypeStr},
        {"from", jlib:jid_to_string(From)},
        {"to", jlib:jid_to_string(To)}],
           [{xmlelement, "body", [],
         [{xmlcdata, BodyStr}]}]},
    ejabberd_router:route(From, To, XmlBody).
    
do(Q) ->
    F = fun() ->
                qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
    
%% main xmpp routing functions
%% might replace with a handle_info/gen_server version

%% presence routing
route(From, To, {xmlelement, "presence", _, _} = Packet) ->
    case xml:get_tag_attr_s("type", Packet) of
        "subscribe" ->
            send_presence(To, From, "subscribe");
        "subscribed" ->
            send_presence(To, From, "subscribed"),
            send_presence(To, From, "");
        "unsubscribe" -> 
            send_presence(To, From, "unsubscribed"),
            send_presence(To, From, "unsubscribe");
        "unsubscribed" ->
            send_presence(To, From, "unsubscribed");
        "" ->
            send_presence(To, From, "");
        "unavailable" ->
            ok;
        "probe" ->
            send_presence(To, From, "");
        _Other ->
            ?INFO_MSG("Other kind of presence~n~p", [Packet])
    end,
    ok;

%% message routing
route(From, To, {xmlelement, "message", _, _} = Packet) ->
    case xml:get_subtag_cdata(Packet, "body") of
    "" ->
        ok;
    Body ->
        case xml:get_tag_attr_s("type", Packet) of

        "error" ->
            ?ERROR_MSG("Received error message~n~p -> ~p~n~p", [From, To, Packet]);
        _ ->
            [Command|Args] = string:tokens(strip_bom(Body), " "),
            case Command of
            "buy" ->
              do_buy(To, From, Args);
            "create-contract" ->
              do_create_contract(From, To, Args);
            "list-contracts" ->
              do_list_contracts(From, To, Args);
            "create-market" ->
              do_create_market(From, To, Args);
            "create-account" ->
              do_create_account(From, To, Args);
            "reset" ->
              mnesia:clear_table(market),
              mnesia:clear_table(account),
              echo(To, From, "Reset complete");
            _ ->
              echo(To, From, Body)
            end
        end
    end,
    ok.

%% process HTTP requests
process(_Path, _Request) ->
  "Foo".



%% CURRENTLY IMPORTANT CODE STARTS HERE  %%

%create_tables() ->
  %  mnesia:delete_table(account),
  %  mnesia:delete_table(market),
  %  mnesia:delete_table(portfolio),

  %  mnesia:create_table(portfolio, [{attributes, record_info(fields, portfolio)}]).

%% MARKETS AND CONTRACTS %%

create_market(Name, Contracts, Creator, Expires) ->
  F = fun() ->
    case mnesia:read({ market, Name }) of
      [ Result ] ->
        mnesia:abort("Market Exists");
      [] ->
        NewMarket = #market{ name=Name, contracts=Contracts, creator=Creator, created_date = calendar:universal_time(), expires_date=Expires },
        mnesia:write(NewMarket)
    end
  end,
  
  mnesia_error_or_true(F).
  
create_contract(MarketName, Name, Description, Quantity) ->
  F = fun() ->
    case mnesia:read({ market, MarketName }) of
      [] ->
        mnesia:abort("Market " ++ MarketName ++ " Not Found");
      [ Market|T ] ->
        Status = Market#market.status,
        case Status of
          open ->
            mnesia:abort("Cannot add Contract to open Market");
          closed ->
            Contracts = Market#market.contracts,
            case contract_exists(Contracts, Name) of
              true ->
                mnesia:abort("Contract with that name already exists ");
              false ->
                NewContract = #contract{ name = Name, description = Description, quantity = Quantity},
                NewContractList = update_prices([NewContract|Contracts]),
                Market2 = Market#market{ contracts = NewContractList },
                mnesia:write(Market2)
            end
        end
    end
  end,
  mnesia_error_or_true(F).
  
get_market(Name) ->
  F = fun() ->
    case mnesia:read({ market, Name }) of
      [] ->
        { error, "Market Not Found "};
      [ Market|T ] ->
        { ok, Market }
    end
  end,
  
  mnesia:transaction(F).
  
contract_exists([C|T], Name) ->
  CName = C#contract.name,
  case CName of
    Name ->
      true;
    _ ->
      contract_exists(T, Name)
  end;
  
contract_exists([], Name) ->
  false.

get_contract([C|T], Name) ->
  CName = C#contract.name,
  case CName of
    Name ->
      C;
    _ ->
      get_contract(T, Name)
  end;

get_contract([], Name) ->
  false.
  
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

%sum_powers(List) ->
%  sum_powers(List, 0).


sum_powers([C|T]) ->
  Quantity = C#contract.quantity,
  math:exp(Quantity/?MSR_B) + sum_powers(T);

sum_powers([]) ->
  0.

%% equivalent to the C method in the python code

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

cost_to_trade(ContractList, ContractName, Quantity) ->
  100 * (cost_at_quantity(ContractList, ContractName, Quantity) - cost_at_quantity(ContractList, ContractName, 0)).

%% ACCOUNTS AND PORTFOLIOS %%


get_account(Owner) ->
  F = fun() ->
    case mnesia:read({ account, Owner }) of
      [Account|_] ->
        Account;
      [] ->
        mnesia:abort("Account Not Found")
    end
  end,
  mnesia:transaction(F).


create_account(Owner, Balance, Portfolio) ->
  F = fun() ->
    case mnesia:read({ account, Owner }) of
      [ Result ] ->
        mnesia:abort("Account for that Owner already Exists");
      [] ->
        NewAccount = #account{ owner=Owner, balance=Balance, created_date = calendar:universal_time(), portfolio=Portfolio },
        mnesia:write(NewAccount)
    end
  end,
  mnesia_error_or_true(F).
  
create_default_account(Owner) ->
  create_account(Owner, 10000, []).

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
  

buy_contracts(AccountOwner, MarketName, ContractName, Quantity) ->
  F = fun() ->
    [Account|_] = mnesia:read({ account, AccountOwner }),
    [Market|_] = mnesia:read({ market, MarketName }),
    
    case contract_exists(Market#market.contracts, ContractName) of
      true ->
        Cost = cost_to_trade(Market#market.contracts, ContractName, Quantity),
        NewBalance = Account#account.balance - Cost,
        if
          NewBalance > 0 ->
            NewPortfolio = add_to_portfolio(MarketName, ContractName, Quantity, Account#account.portfolio),
            NewAccount = Account#account{ balance = NewBalance, portfolio = NewPortfolio },
            NewContractList = update_prices(change_contract_quantity(Market#market.contracts, ContractName, Quantity)),
            NewMarket = Market#market{ contracts = NewContractList },
            mnesia:write(NewMarket),
            mnesia:write(NewAccount);
            
          true ->
            mnesia:abort("Not enough cash in account")
        end;
      _ ->
        mnesia:abort("Contract does not exist")
    end
  end,
  mnesia_error_or_true(F).
  
sell_contracts(AccountOwner, MarketName, ContractName, SellQuantity) ->
  Quantity = 0 - SellQuantity,
  F = fun() ->
    [Account|_] = mnesia:read({ account, AccountOwner }),
    [Market|_] = mnesia:read({ market, MarketName }),
    
    PE = get_portfolio_entry(Account#account.portfolio, MarketName, ContractName),
    
    case PE of
      [] ->
        mnesia:abort("Not enough shares to sell");
      _ ->
        if
          PE#portfolio_entry.quantity >= Quantity ->
            Cost = cost_to_trade(Market#market.contracts, ContractName, Quantity),
            NewBalance = Account#account.balance - Cost,
        
            NewPortfolio = add_to_portfolio(MarketName, ContractName, Quantity, Account#account.portfolio),
            NewAccount = Account#account{ balance = NewBalance, portfolio = NewPortfolio },
            NewContractList = update_prices(change_contract_quantity(Market#market.contracts, ContractName, Quantity)),
            NewMarket = Market#market{ contracts = NewContractList },
            mnesia:write(NewMarket),
            mnesia:write(NewAccount);
          true ->
            mnesia:abort("Not enough shares to sell")
        end
    end
  end,
  mnesia_error_or_true(F).
  
%% UTILITY FUNCTIONS  %%

mnesia_error_or_true(F) ->
  Result = mnesia:transaction(F),
  case Result of
    { atomic, _ } ->
      true;
    _ ->
      Result
  end.
  
string_join(Join, L) ->
    string_join(Join, L, fun(E) -> E end).

string_join(_Join, L=[], _Conv) ->
    L;
string_join(Join, [H|Q], Conv) ->
    lists:flatten(lists:concat(
        [Conv(H)|lists:map(fun(E) -> [Join, Conv(E)] end, Q)]
    )).


%% CURRENTLY IMPORTANT CODE ENDS HERE - EVERYTHING ELSE IS CRUFT %%
  
  
do_list_contracts(User, Me, Args) ->
  if
    length(Args) == 1 ->
      [MarketName|_] = Args,
      GetMarket = get_market(MarketName),
      case GetMarket of
        { atomic, { ok, Market } } ->
          ContractNames = lists:map(fun(X) -> X#contract.name end, Market#market.contracts),
          ContractString = string_join(" ", ContractNames),
          echo(User, Me, ContractString);
        { atomic, { error, Message } } ->
          echo(User, Me, Message)
      end;
    true ->
      echo(User, Me, "Wrong arguments")
  end.

do_create_contract(User, Me, Args) ->
  if
    length(Args) == 2 ->
      [MarketName, ContractName|_] = Args,
      Name = jlib:jid_to_string(User),
      case create_contract(MarketName, ContractName, "asdf", 0) of
        true ->
          echo(User, Me, "Contract created");
        { aborted, Reason } ->
          echo(User, Me, "An error occurred")
      end;
    true ->
      echo(User, Me, "Too few arguments")
  end.
  
  
do_create_market(User, Me, Args) ->
  if
    length(Args) == 1 ->
      [MarketName|_] = Args,
      Name = jlib:jid_to_string(User),
      case create_market(MarketName, [], Name, 0) of
        true ->
          echo(User, Me, "Market created");
        { aborted, Reason } ->
          echo(User, Me, "An error occurred")
      end;
    true ->
      echo(User, Me, "Too few arguments")
  end.
    
do_create_account(User, Me, Args) ->
  Name = jlib:jid_to_string(User),
  case create_default_account(Name) of
    { aborted, Reason } ->
      echo(User, Me, "An error occurred");
    _ ->
      echo(User, Me, "Account created")
  end.

do_buy(User, Me, Args) ->
  if
    length(Args) == 3 ->
      [MarketName, ContractName, QuantityString|_] = Args,
      { Quantity, _ } = string:to_integer(QuantityString),
      case buy_contracts(jlib:jid_to_string(User), MarketName, ContractName, Quantity) of
        true ->
          echo(User, Me, "Contracts bought");
        { aborted, Reason } ->
          echo(User, Me, io_lib:format("~p~n", [[MarketName, ContractName, Quantity]]))
      end;
    true ->
      echo(User, Me, "Too few arguments")
  end.
  
do_sell(User, Me, Args) ->
  if
    length(Args) == 3 ->
      [MarketName, ContractName, QuantityString|_] = Args,
      { Quantity, _ } = string:to_integer(QuantityString),
      case sell_contracts(jlib:jid_to_string(User), MarketName, ContractName, Quantity) of
        true ->
          echo(User, Me, "Contracts sold");
        { aborted, Reason } ->
          echo(User, Me, "An error occurred")
      end;
    true ->
      echo(User, Me, "Too few arguments")
  end.
