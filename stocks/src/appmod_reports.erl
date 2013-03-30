%% appmod for YAWS server. Reports service entry point
-module(appmod_reports).
-include("stocks_info.hrl").
-include("yaws_api.hrl").

-export([out/1]).

%% API

out(A) ->
  Req = A#arg.req,
  Uri = yaws_api:request_url(A),
  %% Path is like this ["reports", "basic"]
  Path = string:tokens(Uri#url.path, "/"),
  appmod(Req#http_request.method, Path, A).

%% Functions

%% processing request from YAWS
%% we accept GET method only for service/reports/basic
appmod('GET', ["reports", "basic"], A) ->
  %% Arguments for report are stored in URL
  P = yaws_api:parse_query(A),
  try
    Request = query_parse(P),
    Answer = stocks_server:get_report(basic, Request),
    {ehtml, [lists:flatten(Answer)]}
  catch 
    Error:Reason ->
      error_logger:info_msg("GET ~p:~p~n",[Error, Reason]), 
      yaws_outmod:out404(A)
  end;
%% deny everything else
appmod(_, _, A) ->
  yaws_outmod:out404(A).

%% look for stocks period info in query.
%% query_parse_get(Query::string()) = report_request()
query_parse(Query) ->
  {"name", Name} = lists:keyfind("name", 1, Query),
  {"datefrom", DateFrom} = lists:keyfind("datefrom", 1, Query),
  {"dateto", DateTo} = lists:keyfind("dateto", 1, Query),
  {"scale", Scale} = lists:keyfind("scale", 1, Query),
     
  #report_request{
    name = Name, 
    datefrom = parse_utils:string_to_datetime(DateFrom), 
    dateto = parse_utils:string_to_datetime(DateTo), 
    scale = parse_utils:scale_to_atom(Scale)}.