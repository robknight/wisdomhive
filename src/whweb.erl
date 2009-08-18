-module(whweb).

-include_lib("stdlib/include/qlc.hrl").
-include("yaws_api.hrl").
-include("whrecords.hrl").

-export([out/1]).

out(Arg) ->
  Uri = yaws_api:request_url(Arg),
  Path = string:tokens(Uri#url.path, "/"),
  Method = (Arg#arg.req)#http_request.method,
  out(Arg, Method, Path).
    

get_single_market(Name) ->
  mnesia:dirty_read({ market, Name }).

list_markets() ->
  ets:tab2list(market).

list_markets_json() ->
  Markets = list_markets(),
  MarketJson = list_markets_json(Markets),
  list_to_binary(mochijson2:encode({ struct, [ { markets, MarketJson }]})).
  
list_markets_json([M|T]) ->
  %Struct = { struct, [ market_json(M) ] },
  [market_json(M)|list_markets_json(T)];

list_markets_json([]) ->
  [].
  
market_json(M) ->
  { struct, [ { name, list_to_binary(M#market.name) }, { status, M#market.status }, { contracts, contracts_to_json(M#market.contracts) } ] }.
  
show_market_json(Market) ->
  list_to_binary(mochijson2:encode(market_json(Market))).
  
contracts_to_json(Contracts) ->
  contract_json(Contracts).
  
contract_json([C|T]) ->
  Name = list_to_binary(C#contract.name),
  Struct = { struct, [ { name, Name }, { description, list_to_binary(C#contract.description) },
    { quantity, C#contract.quantity }, { price, C#contract.price }, { status, C#contract.status }, { final_price, C#contract.final_price } ]},
  [Struct|contract_json(T)];

contract_json([]) ->
  [].

out(Arg, 'GET', ["wh", "markets"]) ->
  [{content, "text/javascript", list_markets_json()}];
  
out(Arg, 'GET', ["wh", "market", MarketName]) ->
  case get_single_market(MarketName) of
    [] ->
      [
        {status, 404}
      ];
    [Market] ->
      [{content, "text/javascript", show_market_json(Market)}]
  end;

%% TODO: error handling/validation  
out(Arg, 'POST', ["wh", "markets"]) ->
  Json = mochijson2:decode(Arg#arg.clidata);
  %Headers = Arg#arg.headers,
  %{ Username, Password, _ } = Headers#headers.authorization,
  
  %[{content, "text/javascript", "Attempted login as " ++ Username ++ " with password " ++ Password }];
      
  %[{content, "text/javascript", show_market_json(MarketName)}];

%out(Arg, 'GET', ["wh", "markets", Name]) ->
%  [{content, "text/javascript", }]
  
out(Arg, 'GET', _) ->
  [
    {status, 200},
    {ehtml,  [{head, [], [{title, [], "hmm"}]},
      {body, [],
       [{h1, [], "asdfwewe"},
        {p, [], "aaa"}]}]}
  ].