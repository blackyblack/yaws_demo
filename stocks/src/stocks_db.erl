-module(stocks_db).
-include("stocks_info.hrl").

-behaviour(gen_server).

-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	     terminate/2, code_change/3]).

-export([add_stock/1, get_stock/1, select_stocks/1]).

%% our data table
-record(main_state, {storage, lastkey}).

%% API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% add stock into ETS DB
%% add_stock(Stock::stock_record()) = Id::integer()
add_stock(Stock) ->
  gen_server:call(?MODULE, {add_stock, Stock}, 5000).

%% get stock from ETS DB
%% get_stock(Id::integer()) = [{Id, stock_record()}]
get_stock(Id) ->
  gen_server:call(?MODULE, {get_stock, Id}, 5000).

%% select stock info by time period
%% select_stocks(Request::report_request()) = 
%%   [{Time::datetime(), [stock_record()]}]
select_stocks(Request) ->
  gen_server:call(?MODULE, {select_stocks, Request}, 15000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
  State = #main_state{
            storage = ets:new(stocks, []),
            lastkey = 0},
  {ok, State}.

%% put record in db
handle_call({add_stock, Stock}, _From, 
            State = #main_state{storage = DB, lastkey = Id}) ->
  %% put data into main db
  ets:insert(DB, {Id, Stock}),
  {reply, Id,  State#main_state{lastkey = Id + 1}};

%% get record from db
handle_call({get_stock, Id}, _From, State = #main_state{storage = DB}) ->
  {reply, ets:lookup(DB, Id), State};

%% select records from db
%% Request::report_request()
%% Reply = [{Time::datetime(), [stock_record()]}]
handle_call({select_stocks, Request}, _From, State) ->
  %% we've got keys for selected scale dates.
  DataList = select_by_request(Request, State),
  {reply, DataList, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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

%% selects data about transactions with conditions from Request.
%% returns [{Time::datetime(), [stock_record()]}]
select_by_request(#report_request{
                    datefrom = From, dateto = To, scale = Scale, name = Name}, 
                  State) ->
  S = calendar:datetime_to_gregorian_seconds(From),
  E = calendar:datetime_to_gregorian_seconds(To),
  select_by_request(Name, scale_to_fun(Scale), S, E, [], State).

select_by_request(_Name, _ScaleFun, Pos, End, AccList, _State)
  when (Pos >= End) ->
    lists:reverse(AccList);

select_by_request(Name, ScaleFun, Pos, End, AccList, State) ->
  %% take interval size
  Interval = ScaleFun(Pos),
  EndInterval = Pos + Interval,
  %% Take raw data from interval start time to interval end
  %% with name = Name
  DbData = ets:select(
    State#main_state.storage, [{
      {'_', #stock_record{name = Name, datetime = '$1', _ = '_'}},
      [
        %% guards: time >= Pos and time =< EndInterval
        {'>=', '$1', Pos},
        {'<', '$1', EndInterval}],
      ['$_']}]),
  %% convert list of tuples into list of stock_record()
  Data = [X || {_Id, X} <- DbData],
  %% add information about start of the period
  case Data of
    [] -> 
      AccListOut = AccList;
    _ -> 
      AccListOut = [{Pos, Data}] ++ AccList
  end,
  select_by_request(Name, ScaleFun, EndInterval, End, AccListOut, State).

%% various funs() used for split_lists_with_variable_interval()
%% we cannot do it simplier since we need month (or year) scale.
scale_to_fun(minute) -> fun(_) -> 60 end;
scale_to_fun(hour) -> fun(_) -> 60 * 60 end;
scale_to_fun(day) -> fun(_) -> 60 * 60 * 24 end;
scale_to_fun(week) -> fun(_) -> 60 * 60 * 24 * 7 end;
scale_to_fun(month) -> 
  fun(Arg) -> 
    DateTimeStart = calendar:gregorian_seconds_to_datetime(Arg),
    {{Year, Month, _}, _} = DateTimeStart,
    LastDay = calendar:last_day_of_the_month(Year, Month),
    (60 * 60 * 24 * LastDay) 
  end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

database_test() ->
  stocks_db:start_link(),
  
  R = #stock_record{
        name = "x", datetime = 100, price = 1.0, count = 10},
  Id = stocks_db:add_stock(R),
  ?assertEqual(Id, 0),
  D = stocks_db:get_stock(Id),
  ?assertEqual([{Id, R}], D),
  D1 = stocks_db:get_stock(Id + 1),
  ?assertEqual([], D1),
  Id1 = stocks_db:add_stock(R),
  ?assertEqual(Id1, 1),
  
  stocks_db:stop().

select_test() ->
  stocks_db:start_link(),

  Req1 = #report_request{
           name = "x", datefrom = {{2000,1,1},{0,0,0}}, 
           dateto = {{2000,1,1},{1,0,0}}, scale=hour},
  Res1 = stocks_db:select_stocks(Req1),
  ?assertEqual(Res1, []),

  Sec1 = calendar:datetime_to_gregorian_seconds({{2000,1,1},{0,0,0}}),
  R1 = #stock_record{
         name = "x", datetime = Sec1, 
         price = 1.0, count = 10},
  _Id1 = stocks_db:add_stock(R1),
  Sec2 = calendar:datetime_to_gregorian_seconds({{2000,1,1},{0,1,0}}),
  R2 = #stock_record{
         name = "x", datetime = Sec2, 
         price = 1.0, count = 10},
  _Id2 = stocks_db:add_stock(R2),

  Req2 = #report_request{
           name = "x", datefrom = {{2000,1,1},{0,0,0}}, 
           dateto = {{2000,1,1},{1,0,0}}, scale=hour},
  Res2 = stocks_db:select_stocks(Req2),
  [{P, Data}] = Res2,
  TimeSorted = lists:sort(
    fun(A, B) -> 
      A#stock_record.datetime =< B#stock_record.datetime end,
    Data),
  ?assertEqual([{P, TimeSorted}], [{Sec1, [R1, R2]}]),
  
  stocks_db:stop().  

-endif.