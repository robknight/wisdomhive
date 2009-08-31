-module(whjson).

-include("wh_commands.hrl").

-compile(export_all).

process_string(JsonString) ->
  Command = mochijson2:decode(JsonString),
  process_command(Command).

extract_param(Name, [H|T]) ->
  { CurrentName, Data } = H,
  case CurrentName of
    Name ->
      Data;
    _ ->
      extract_param(Name, T)
  end;

extract_param(Name, []) ->
  throw("Param not found: " ++ Name).

match_params([Param|ParamList], Data) ->
  [extract_param(Param, Data)|match_params(ParamList, Data)];

match_params([], _Data) ->
  [].

create_command(Type, ParamList, Data) ->
  List = [Type|match_params(ParamList, Data)],
  list_to_tuple(List).

process_command(Json) ->
  case process_json_object(Json) of
    { buy, Params } ->
      ParamList = record_info(fields, buy),
      create_command(buy, ParamList, Params)
  end.
  
%compose_record(Type, Fields) ->
  

%execute_command({ buy, { Parameters }}) ->
  
  
process_json_object(Json) ->
  { struct, Object } = Json,
  { { Name, { struct, Content } } } = list_to_tuple(Object),
  { list_to_atom(binary_to_list(Name)), lists:map(fun(X) -> convert_term(X) end, Content) }.

% at present, commands only send simple values
% we might want to improve this if the parameters contained in the json objects
% may themselves be objects
% NOTE: the use of list_to_existing_atom serves as validation of sorts, albeit weak
convert_term(Term) ->
  { Name, Content } = Term,
  NewContent = if
    is_binary(Content) ->
      binary_to_list(Content);
    true ->
      Content
  end,
  { list_to_existing_atom(binary_to_list(Name)), NewContent }.