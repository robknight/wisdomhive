-module(whbot).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile(export_all).

-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

-include("../include/wh_commands.hrl").
-include("../include/wh_events.hrl").
-include("../include/whrecords.hrl").
-include("../include/whbot.hrl").

-define(COMPONENT, "market.whtest.local").
-define(SERVER_HOST, "localhost").
-define(SERVER_PORT, 7776).
-define(SECRET, "secret").


-record(state, {
	session
	}).
  
-record(command_info, {
  name, description, rec, rec_info
}).
  
get_command_info(Name) ->
  case Name of
    <<"buy">> -> #command_info{ name = Name, description = <<"Buy Contracts">>,
                                rec = #buy{}, rec_info = record_info(fields, buy) };
    <<"sell">> -> #command_info{ name = Name, description = <<"Sell Contracts">>,
                                rec = #sell{}, rec_info = record_info(fields, sell) };
    <<"create_market">> -> #command_info{ name = Name, description = <<"Create Market">>,
                                rec = #create_market{}, rec_info = record_info(fields, create_market) };
    <<"create_contract">> -> #command_info{ name = Name, description = <<"Create Contract">>,
                                rec = #create_contract{}, rec_info = record_info(fields, create_contract) };
    <<"open_market">> -> #command_info{ name = Name, description = <<"Open Market">>,
                                rec = #open_market{}, rec_info = record_info(fields, open_market) };
    <<"close_market">> -> #command_info{ name = Name, description = <<"Close Market">>,
                                rec = #close_market{}, rec_info = record_info(fields, close_market) }
  end.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
	gen_server:call(?MODULE, stop).

-spec init([]) -> {ok, #state{}}.
init([]) ->
	Session = exmpp_component:start_link(),
	exmpp_component:auth(Session, ?COMPONENT, ?SECRET),
	_StreamId = exmpp_component:connect(Session, ?SERVER_HOST, ?SERVER_PORT),
	ok = exmpp_component:handshake(Session),
  wh_event_manager:add_handler(wh_bot_events, [Session]),
	{ok, #state{session = Session}}.

-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}} | {noreply, #state{}}.
handle_call(stop, _From, State) ->
	exmpp_component:stop(State#state.session),
	{stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
	{reply, unexpected, State}.
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(#received_packet{} = Packet, #state{session = S} = State) ->
	spawn_link(fun() -> process_received_packet(S, Packet) end),
	{noreply, State};
handle_info(#received_packet{packet_type=Type, raw_packet=Packet}, State) ->
	error_logger:warning_msg("Unknown packet received(~p): ~p", [Type, Packet]),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.


-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
	{noreply, State}.
-spec terminate(any(), #state{}) -> any().
terminate(_Reason, _State) ->
	ok.
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


process_received_packet(Session, #received_packet{packet_type = 'iq', type_attr=Type, raw_packet = IQ}) ->
	process_iq(Session, Type, exmpp_xml:get_ns_as_atom(exmpp_iq:get_payload(IQ)), IQ);
  
process_received_packet(Session, #received_packet{packet_type = 'presence', type_attr = Type, raw_packet = Presence, from = From}) ->
  process_presence(Session, Type, Presence, From);

process_received_packet(_Session, Packet) ->
	io:format("Unknown packet: ~p\n", [Packet]).

process_presence(Session, "subscribe", Presence, From) ->
  Subscribed1 = exmpp_xml:set_attribute(exmpp_presence:subscribed(), to, From),
  exmpp_session:send_packet(Session, Subscribed1);

process_presence(_Session, _Type, _Presence, _From) ->
  ok.

process_iq(Session, "get", ?NS_DISCO_INFO, IQ) ->
  Node = exmpp_xml:get_attribute_as_binary(exmpp_iq:get_payload(IQ), 'node', <<"">>),
	Result = disco_info(Node, IQ),
	exmpp_component:send_packet(Session, Result);

process_iq(Session, "get", ?NS_DISCO_ITEMS, IQ) ->
  Node = exmpp_xml:get_attribute_as_binary(exmpp_iq:get_payload(IQ), 'node', <<"">>),
	Result = disco_items(Node, IQ),
	exmpp_component:send_packet(Session, Result);
  
process_iq(Session, "get", ?NS_INBAND_REGISTER, IQ) ->
  %From = exmpp_jid:parse(exmpp_stanza:get_sender(IQ)),
	Result = exmpp_iq:result(IQ),
	exmpp_component:send_packet(Session, Result);

process_iq(Session, "set", ?NS_INBAND_REGISTER, IQ) ->
	From = exmpp_jid:parse(exmpp_stanza:get_sender(IQ)),
	Command = #create_account{user = exmpp_jid:prep_bare_to_list(From) },
	ok = whmarket:create_account(Command),
	exmpp_component:send_packet(Session, exmpp_iq:result(IQ));
  
process_iq(Session, "set", ?NS_ADHOC, IQ) ->
  io:format("received command ~n~p", [IQ]),
  XmlCommand = exmpp_iq:get_payload(IQ),
  User = exmpp_jid:prep_bare_to_list(exmpp_jid:parse(exmpp_stanza:get_sender(IQ))),
  Node = exmpp_xml:get_attribute_as_list(XmlCommand, 'node', "none"),
  case exmpp_xml:get_element(XmlCommand, ?NS_DATA_FORMS, 'x') of
    undefined ->
      #command_info{ rec = Record } = get_command_info(list_to_binary(Node)),
      RForm = make_form(Record),
      	Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_ADHOC, 'command',
          [exmpp_xml:attribute('node', Node), exmpp_xml:attribute('status', <<"executing">>)], 
          [RForm])),
        exmpp_component:send_packet(Session, Result);
    Form ->
      FieldElements = exmpp_xml:get_elements(Form,  ?NS_DATA_FORMS, 'field'),
      Fields = lists:map(fun(Field) ->
            {exmpp_xml:get_attribute_as_binary(Field, 'var', <<>>), 
             exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Field, 'value'))}
            end, FieldElements),
      
      case parse_command(XmlCommand, Node, Fields, User) of
        unknown_command ->
          io:format("unknown command~n"),
          blah;
        badly_formed ->
          io:format("badly formed command~n"),
          blah;
        Command ->
            case whmarket:execute(Command) of
              {error, badly_formed} ->
                io:format("badly formed command~n");
              { error, Reason } ->
                ok; % TODO: here we need to send an error message to the client
              ok ->
                Result = exmpp_iq:result(IQ,  exmpp_xml:element(?NS_ADHOC, 'command',
                  [exmpp_xml:attribute('node', Node), exmpp_xml:attribute('status', <<"completed">>)],
                  [])),
                exmpp_component:send_packet(Session, Result)
            end
      end
  end;
    
process_iq(_Session, _, _, IQ) ->
  From = exmpp_jid:parse(exmpp_stanza:get_sender(IQ)),
	io:format("unknown IQ received from ~p:~n", [[From, exmpp_xml:get_attribute_as_binary(exmpp_iq:get_payload(IQ), 'node', <<"">>)]]),
  ok.
  
  
 
disco_items(?NS_ADHOC_b, IQ) ->
  Commands = [{<<"buy">>, <<"Buy Contracts">>}, {<<"sell">>, <<"Sell Contracts">> },
              {<<"create_contract">>, <<"Create Contract">> },
              {<<"create_market">>, <<"Create Market">> },
              {<<"open_market">>, <<"Open Market">>},
              {<<"close_market">>, <<"Close Market">>}],
  Children = [
    #xmlel{name = item,
			attrs = [
					 #xmlattr{name = jid, value = list_to_binary(?COMPONENT)},
					 #xmlattr{name = name, value = Name},
           #xmlattr{name = node, value = Node}
					]
		   } || { Node, Name } <- Commands
	],
  Result = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [exmpp_xml:attribute('node', ?NS_ADHOC_s)], Children),
  exmpp_iq:result(IQ, Result);
  
disco_items(<<"">>, IQ) ->
  Result = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], []),
  exmpp_iq:result(IQ, Result).

disco_info(<<"">>, IQ) ->
	Identity = exmpp_xml:element(?NS_DISCO_INFO, 'identity', [exmpp_xml:attribute("category", <<"component">>),
				  		      exmpp_xml:attribute("type", <<"generic">>),
						      exmpp_xml:attribute("name", <<"WH Market">>)
						      ], 
			 	     []),
	IQRegisterFeature = exmpp_xml:element(?NS_DISCO_INFO, 'feature', [exmpp_xml:attribute('var', ?NS_INBAND_REGISTER_s)],[]),
	CommandsFeature = exmpp_xml:element(?NS_DISCO_INFO, 'feature', [exmpp_xml:attribute('var', ?NS_ADHOC_s)], []),
 % PubSubFeature = exmpp_xml:element(?NS_DISCO_INFO, 'feature', [exmpp_xml:attribute('var', ?NS_PUBSUB_s)], []),
  
	exmpp_iq:result(IQ, exmpp_xml:element(?NS_DISCO_INFO, 'query', [],
    [Identity, IQRegisterFeature, CommandsFeature])).
 

  
parse_command(_XmlCommand, "buy", Fields, User) ->
  try lists:foldl(
  fun( {<<"market_name">>, Value}, #buy{} = Cmd) ->
				Cmd#buy{ market_name = Value };
		  ({<<"contract_name">>, Value}, #buy{} = Cmd) ->
				Cmd#buy{ contract_name = Value };
      ({<<"quantity">>, Value}, #buy{} = Cmd) ->
        Cmd#buy{ quantity = list_to_integer(Value) };
      ({<<"max_price">>, Value}, #buy{} = Cmd) ->
        Cmd#buy{ max_price = list_to_integer(Value) };
      (_, _) ->
        throw(badly_formed)
	end, #buy{ user = User }, Fields)
  catch
    Exception -> Exception
  end;

parse_command(_XmlCommand, "sell", Fields, User) ->
  try lists:foldl(
  fun( {<<"market_name">>, Value}, #sell{} = Cmd) ->
				Cmd#sell{ market_name = Value };
		  ({<<"contract_name">>, Value}, #sell{} = Cmd) ->
				Cmd#sell{ contract_name = Value };
      ({<<"quantity">>, Value}, #sell{} = Cmd) ->
        Cmd#sell{ quantity = list_to_integer(Value) };
      ({<<"min_price">>, Value}, #sell{} = Cmd) ->
        Cmd#sell{ min_price = list_to_integer(Value) };
      (_, _) ->
        throw(badly_formed)
	end, #sell{ user = User }, Fields)
  catch
    Exception -> Exception
  end;
  
parse_command(_XmlCommand, "create_contract", Fields, User) ->
  try lists:foldl(
  fun( {<<"market_name">>, Value}, #create_contract{} = Cmd) ->
				Cmd#create_contract{ market_name = Value };
		  ({<<"contract_name">>, Value}, #create_contract{} = Cmd) ->
				Cmd#create_contract{ contract_name = Value };
      ({<<"description">>, Value}, #create_contract{} = Cmd) ->
        Cmd#create_contract{ description = Value };
      (_, _) ->
        throw(badly_formed)
	end, #create_contract{ user = User }, Fields)
  catch
    Exception -> Exception
  end;
  
parse_command(_XmlCommand, "create_market", Fields, User) ->
  try lists:foldl(
  fun( {<<"market_name">>, Value}, #create_market{} = Cmd) ->
				Cmd#create_market{ market_name = Value };
      ({<<"description">>, Value}, #create_market{} = Cmd) ->
        Cmd#create_market{ description = Value };
      (_, _) ->
        throw(badly_formed)
	end, #create_market{ user = User }, Fields)
  catch
    Exception -> Exception
  end;

parse_command(_XmlCommand, "open_market", Fields, User) ->
  try lists:foldl(
  fun( {<<"market_name">>, Value}, #open_market{} = Cmd) ->
				Cmd#open_market{ market_name = Value };
      (_, _) ->
        throw(badly_formed)
	end, #open_market{ user = User }, Fields)
  catch
    Exception -> Exception
  end;
  
parse_command(_XmlCommand, "close_market", Fields, User) ->
  try lists:foldl(
  fun( {<<"market_name">>, Value}, #close_market{} = Cmd) ->
				Cmd#close_market{ market_name = Value };
      (_, _) ->
        throw(badly_formed)
	end, #close_market{ user = User }, Fields)
  catch
    Exception -> Exception
  end;

parse_command(_XmlCommand, _, _Fields, _User) ->
  unknown_command.
  
make_form(#buy{} = Data) ->
  MarketNameField = ?FIELD(<<"text-single">>, <<"market_name">>, <<"Market Name">>, list_to_binary(Data#buy.market_name)),
  ContractNameField = ?FIELD(<<"text-single">>, <<"contract_name">>, <<"Contract Name">>, list_to_binary(Data#buy.contract_name)),
  QuantityField = ?FIELD(<<"text-single">>, <<"quantity">>, <<"Quantity">>, integer_to_binary(Data#buy.quantity)),
  MaxPriceField = ?FIELD(<<"text-single">>, <<"max_price">>, <<"Maximum Price">>, integer_to_binary(Data#buy.max_price)),
  exmpp_xml:element(?NS_DATA_FORMS, 'x', [?XMLATTR(type,<<"form">>)], 
					[MarketNameField, ContractNameField, QuantityField, MaxPriceField]);

make_form(#sell{} = Data) ->
  MarketNameField = ?FIELD(<<"text-single">>, <<"market_name">>, <<"Market Name">>, list_to_binary(Data#sell.market_name)),
  ContractNameField = ?FIELD(<<"text-single">>, <<"contract_name">>, <<"Contract Name">>, list_to_binary(Data#sell.contract_name)),
  QuantityField = ?FIELD(<<"text-single">>, <<"quantity">>, <<"Quantity">>, integer_to_binary(Data#sell.quantity)),
  MinPriceField = ?FIELD(<<"text-single">>, <<"max_price">>, <<"Minimum Price">>, integer_to_binary(Data#sell.min_price)),
  exmpp_xml:element(?NS_DATA_FORMS, 'x', [?XMLATTR(type,<<"form">>)], 
					[MarketNameField, ContractNameField, QuantityField, MinPriceField]);
          
make_form(#create_market{} = Data) ->
  MarketNameField = ?FIELD(<<"text-single">>, <<"market_name">>, <<"Market Name">>, list_to_binary(Data#create_market.market_name)),
  DescriptionField = ?FIELD(<<"text-single">>, <<"description">>, <<"Description">>, list_to_binary(Data#create_market.description)),
  exmpp_xml:element(?NS_DATA_FORMS, 'x', [?XMLATTR(type,<<"form">>)], 
					[MarketNameField, DescriptionField]);

make_form(#create_contract{} = Data) ->
  MarketNameField = ?FIELD(<<"text-single">>, <<"market_name">>, <<"Market Name">>, list_to_binary(Data#create_contract.market_name)),
  ContractNameField = ?FIELD(<<"text-single">>, <<"contract_name">>, <<"Contract Name">>, list_to_binary(Data#create_contract.contract_name)),
  DescriptionField = ?FIELD(<<"text-single">>, <<"description">>, <<"Description">>, list_to_binary(Data#create_contract.description)),
  exmpp_xml:element(?NS_DATA_FORMS, 'x', [?XMLATTR(type,<<"form">>)], 
					[MarketNameField, ContractNameField, DescriptionField]);

make_form(#open_market{} = Data) ->
  MarketNameField = ?FIELD(<<"text-single">>, <<"market_name">>, <<"Market Name">>, list_to_binary(Data#open_market.market_name)),
  exmpp_xml:element(?NS_DATA_FORMS, 'x', [?XMLATTR(type,<<"form">>)], 
					[MarketNameField]);

make_form(#close_market{} = Data) ->
  MarketNameField = ?FIELD(<<"text-single">>, <<"market_name">>, <<"Market Name">>, list_to_binary(Data#close_market.market_name)),
  exmpp_xml:element(?NS_DATA_FORMS, 'x', [?XMLATTR(type,<<"form">>)], 
					[MarketNameField]).
          
integer_to_binary(Int) ->
  list_to_binary(integer_to_list(Int)).