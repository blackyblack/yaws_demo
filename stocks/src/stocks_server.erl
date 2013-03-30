-module(stocks_server).
-include("stocks_info.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([add_stock/1, get_report/2]).
%% For debug only. Do not use.
-export([basic_report/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	     terminate/2, code_change/3]).

%% API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).
  
%% called from YAWS with stock_record() data
add_stock(Stock) ->
  gen_server:call(?MODULE, {add_stock, Stock}).

%% called from YAWS with report_request() data
%% Type = basic  
get_report(Type, Request) ->
  gen_server:call(?MODULE, {get_report, {Type, Request}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% Preparing ets since it's not ours
init([]) ->
  {ok, []}.

%% Just put data into storage
handle_call({add_stock, Stock}, _From, State) ->
  StockInfo = stocks_db:add_stock(Stock),
  Reply = io_lib:fwrite("{id, ~w}", [StockInfo]),
  {reply, Reply, State};
%% Create basic report on stocks.
handle_call({get_report, {basic, _Request} = ReportInfo}, _From, State) ->
  Debug = application:get_env(stocks, debug_type),

  %% taking stocks from storage by request
  Stocks = get_stocks(ReportInfo, Debug),
  Report = create_report(ReportInfo, Stocks, Debug),
  Print = pretty_print(ReportInfo, Report),

  {reply, Print, State};
%% other messages
handle_call(_Request, _From, State) ->
  {reply, [], State}.

handle_cast(stop, State) ->
  exit(normal),
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Functions
   
%% taking stocks data from storage
%% debug version of get_stocks().
get_stocks({basic, Request}, {ok, debug}) ->
  {Time, Value} = timer:tc(stocks_db, select_stocks, [Request]),
  error_logger:info_msg(
    "stocks_db:select_stocks on request ~p took ~p ms~n", 
    [Request, Time / 1000]),
  Value;
%% just redirect request to stocks storage 
get_stocks({basic, Request}, _Debug) ->
  stocks_db:select_stocks(Request).
  
%% create report based on stocks data
%% debug version of get_report().
create_report({basic, Request}, Data, {ok, debug}) ->
  {Time, Value} = timer:tc(?MODULE, basic_report, [Data]),
  error_logger:info_msg(
    "create_basic_report on request ~p took ~p ms~n", 
    [Request, Time / 1000]),
  Value;
create_report({basic, _Request}, Data, _Debug) ->
  basic_report(Data).

%% Report construction functions. 
%% Place in another module when grow to bigger scale

%% create basic report based on scaled data
%% basic_report(ScaledData::[{Time, DataList}]) = [report_line()]
basic_report(ScaledData) ->
  L = lists:foldl(
    fun(A, Acc) -> [basic_report_line(A)] ++ Acc end,
    [], ScaledData),
  lists:reverse(L).

%% basic_report_line({Time::datetime(), DataList::[stock_record()]}) =
%%   report_line() | []
basic_report_line({_Time, []}) ->
  [];
basic_report_line({Time, DataList}) ->
  PriceSorted = lists:sort(
    fun(A, B) -> 
      A#stock_record.price =< B#stock_record.price end,
    DataList),
  TimeSorted = lists:sort(
    fun(A, B) -> 
      A#stock_record.datetime =< B#stock_record.datetime end,
    DataList),
  %% we count total volume of transactions
  Volume = lists:foldr(
    fun(#stock_record{count = Count}, Acc) -> 
      Acc + Count end, 
    0, PriceSorted),
  %% create line of the report
  #report_line{
    timeopen = calendar:gregorian_seconds_to_datetime(Time),
    priceopen = (lists:nth(1, TimeSorted))#stock_record.price,
    priceclose = (lists:last(TimeSorted))#stock_record.price,
    pricemin = (lists:nth(1, PriceSorted))#stock_record.price,
    pricemax = (lists:last(PriceSorted))#stock_record.price,
    volume = Volume}.

%% prepare report for print or serialize
%% pretty_print({basic, _Request}, Report::[report_line()]) = string()
pretty_print({basic, _Request}, []) ->
  "[]";
pretty_print({basic, _Request}, Report) ->
  First = lists:nth(1, Report),
  Last = lists:last(Report),

  lists:map(
    fun(#report_line{
          timeopen = Time, 
          priceopen = PriceOpen, 
          priceclose = PriceClose, 
          pricemin = PriceMin, 
          pricemax = PriceMax, 
          volume = Volume} = R) ->
      if
        R == First -> S = "[";
        true -> S = ""
      end,
      if
        R == Last -> E = "]";
        true -> E = ", "
      end,
      io_lib:fwrite(
        "~s{{timeopen, ~p}, {priceopen, ~.2f}, {priceclose, ~.2f}, " ++
        "{pricemin, ~.2f}, {pricemax, ~.2f}, {totalvolume, ~w}}~s",
        [S, Time, PriceOpen, PriceClose, PriceMin, PriceMax, Volume, E]) end,
    Report).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_stock_test() ->
  stocks_db:start_link(),
  stocks_server:start_link(),
  
  R = #stock_record{
        name = "x", datetime = 100, 
        price = 1.0, count = 10},
  Id = stocks_server:add_stock(R),
  ?assertEqual("{id, 0}", lists:flatten(Id)),
  Id1 = stocks_server:add_stock(R),
  ?assertEqual("{id, 1}", lists:flatten(Id1)),
  
  stocks_db:stop(),
  stocks_server:stop().

get_report_test() ->
  stocks_db:start_link(),
  stocks_server:start_link(),

  Req1 = #report_request{
           name = "x", datefrom = {{2000,1,1},{0,0,0}}, 
           dateto = {{2000,1,1},{1,0,0}}, scale=hour},
  Res1 = stocks_server:get_report(basic, Req1),
  ?assertEqual("[]", lists:flatten(Res1)),

  Sec1 = calendar:datetime_to_gregorian_seconds({{2000,1,1},{0,0,0}}),
  R1 = #stock_record{
         name = "x", datetime = Sec1, 
         price = 1.0, count = 10},
  _Id1 = stocks_server:add_stock(R1),
  Sec2 = calendar:datetime_to_gregorian_seconds({{2000,1,1},{0,1,0}}),
  R2 = #stock_record{
         name = "x", datetime = Sec2, 
         price = 2.0, count = 10},
  _Id2 = stocks_server:add_stock(R2),

  Req2 = #report_request{
           name = "x", datefrom = {{2000,1,1},{0,0,0}}, 
           dateto = {{2000,1,1},{1,0,0}}, scale=hour},
  Res2 = stocks_server:get_report(basic, Req2),
  ?assertEqual("[{{timeopen, {{2000,1,1},{0,0,0}}}, {priceopen, 1.00}, " ++ 
               "{priceclose, 2.00}, {pricemin, 1.00}, {pricemax, 2.00}, " ++
               "{totalvolume, 20}}]", lists:flatten(Res2)),

  Req3 = #report_request{
           name = "x", datefrom = {{2000,1,1},{0,0,0}}, 
           dateto = {{2000,1,1},{1,0,0}}, scale=minute},
  Res3 = stocks_server:get_report(basic, Req3),
  ?assertEqual("[{{timeopen, {{2000,1,1},{0,0,0}}}, {priceopen, 1.00}, " ++ 
               "{priceclose, 1.00}, {pricemin, 1.00}, {pricemax, 1.00}, " ++
               "{totalvolume, 10}}, " ++
               "{{timeopen, {{2000,1,1},{0,1,0}}}, {priceopen, 2.00}, " ++ 
               "{priceclose, 2.00}, {pricemin, 2.00}, {pricemax, 2.00}, " ++
               "{totalvolume, 10}}]", lists:flatten(Res3)),
               
  stocks_db:stop(),
  stocks_server:stop().

-endif.