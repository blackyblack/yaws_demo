%% appmod for YAWS server. Main service entry point.
%% We use erlang syntax for POSTing data and GETtin results.
%% Could use JSON for non erlang clients.
-module(appmod_stocks).
-include("stocks_info.hrl").
-include("yaws_api.hrl").

-export([out/1]).

%% API

out(A) ->
  Req = A#arg.req,
  Uri = yaws_api:request_url(A),
  %% Path is like this ["stocks"]
  Path = string:tokens(Uri#url.path, "/"),
  appmod(Req#http_request.method, Path, A).

%% Functions

%% processing request from YAWS
%% we accept POST method only for service/stocks/
appmod('POST', ["stocks"], A) ->
  %% we do not want to see our server crush on client side
  try
    %% parsing content of the POST request
    Stock = query_parse(A#arg.clidata),
    Data = stocks_server:add_stock(Stock),
    {ehtml, [lists:flatten(Data)]}
  catch
    Error:Reason ->
      error_logger:info_msg("POST ~p:~p~n",[Error, Reason]), 
      yaws_outmod:out404(A)
  end;
%% deny everything else
appmod(_, _, A) ->
  yaws_outmod:out404(A).

%% look for stock info in query.
%% query_parse_put(Query::string()) = stock_record()
query_parse(Query) ->
  Terms = parse_utils:parse_erlang(binary_to_list(Query)),

  {name, Name} = lists:keyfind(name, 1, Terms),
  {datetime, Time} = lists:keyfind(datetime, 1, Terms),
  {price, Price} = lists:keyfind(price, 1, Terms),
  {count, Count} = lists:keyfind(count, 1, Terms),

  R = #stock_record{
        name = Name, 
        datetime = calendar:datetime_to_gregorian_seconds(Time), 
        price = Price * 1.0, 
        count = Count},

  parse_utils:validate_record(R).