-module(http_generator).

-behaviour(gen_server).

%% API

-export([start/0, start/1, start/2, start/3, start/4, stop/0]).

-export([start_put_requests/0]).
-export([get_request/0, get_request/4]).
-export([put_request/0, put_request/4]).

-export([start_loop/2, send_loop/1]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_INTERVAL_MS, 1000).
-define(DEFAULT_MAX_INTEGER, 1000).
-define(DEFAULT_NUM_GENERATORS, 10).
-define(DEFAULT_SERVER, "http://localhost:8080").

-record(state, {
          server,       %% server where we send requests
          num_generators,  %% how many generators to run
          max_integer,     %% maximum value for price and count in stock
          max_interval_ms, %% maximum pause between requests
          owner}).         %% who can stop us

%% API

%% start with settings
start() ->
   start(?DEFAULT_SERVER).

start(Server) ->
   start(Server, ?DEFAULT_NUM_GENERATORS).

start(Server, NumGenerators) ->
   start(Server, NumGenerators, ?DEFAULT_MAX_INTEGER).

start(Server, NumGenerators, MaxInteger) ->
   start(Server, NumGenerators, MaxInteger, ?DEFAULT_INTERVAL_MS).

start(Server, NumGenerators, MaxInteger, MaxIntervalMs) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, 
                          [Server, NumGenerators, MaxInteger, MaxIntervalMs], 
                          []).

stop() ->
   gen_server:cast(?MODULE, stop).

%% tell generators to start sending requests
start_put_requests() ->
   gen_server:cast(?MODULE, start_put_requests).

%% ask server for report
get_request() ->
   gen_server:cast(?MODULE, 
                   {get_request, 
                     {"Test", 
                       {{2011,1,1}, {0,0,0}}, 
                       {{2011,1,2}, {0,0,0}}, 
                       hour}}).

get_request(Name, DateFrom, DateTo, Scale) ->
   gen_server:cast(?MODULE, {get_request, {Name, DateFrom, DateTo, Scale}}).

%% put data to the server
put_request() ->
   gen_server:cast(?MODULE, 
                   {put_request, {"Test", {{2011,1,1}, {0,0,0}}, 100.0, 10}}).

put_request(Name, DateTime, Price, Count) ->
   gen_server:cast(?MODULE, {put_request, {Name, DateTime, Price, Count}}).

%% gen_server api

init([Server, NumGenerators, MaxInteger, MaxIntervalMs]) -> 
    inets:start(),
    State = #state{
              server = Server, 
              num_generators = NumGenerators, 
              max_integer = MaxInteger, 
              max_interval_ms = MaxIntervalMs,
              owner = self()},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    exit(normal),
    {stop, normal, State};

%% start put data
handle_cast(start_put_requests, State) ->
    %% spawn  State#state.num_generators
    Fun = fun() ->
      Now = now(),
      ?MODULE:start_loop(State, Now) end,
    TheList = lists:seq(1, State#state.num_generators),
    
    lists:map(fun(A) -> spawn_link(Fun), A end, TheList),
    {noreply, State};

%% get data from the server
handle_cast({get_request, _}, State) when State#state.server == undefined ->
    {noreply, State};
handle_cast({get_request, What}, State) ->
    {Name, DateFrom, DateTo, Scale} = What,
    Request = io_lib:fwrite("~s?name=~s&datefrom=~w&dateto=~w&scale=~p", 
      [State#state.server ++ "/reports/basic", Name, DateFrom, DateTo, Scale]),
    Request2 = lists:flatten(Request),

    Result = httpc:request(get, {Request2, []}, [], []),
    
    io:format("Get result: ~p~n", [Result]),
    {noreply, State};

%% put data on the server
handle_cast({put_request, _}, State) when State#state.server == undefined ->
    {noreply, State};
handle_cast({put_request, What}, State) ->
    {Name, DateTime, Price, Count} = What,

    Request = io_lib:fwrite(
      "[{name,\"~s\"},{datetime,~w},{price,~.2f},{count,~w}]",
      [Name, DateTime, Price, Count]), 

    Content = lists:flatten(Request),

    io:format("Request is: ~s~n", [Content]),
    
    Result = httpc:request(post, 
               {State#state.server ++ "/stocks", [], "html/txt", Content}, 
               [], []),

    io:format("POST result: ~p~n", [Result]),
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Functions

%% working loop of the single generator
start_loop(State, {A1, A2, A3}) ->
   process_flag(trap_exit, true),
   random:seed(A1, A2, A3),
   random:uniform(10),
   send_loop(State).

send_loop(State) ->
   Price = State#state.max_integer * random:uniform(),
   Count = trunc(State#state.max_integer * random:uniform()),
   
   http_generator:put_request("Test", calendar:now_to_datetime(now()), Price, Count),
   
   Interval = random:uniform(State#state.max_interval_ms),
   Parent = State#state.owner,
   receive
        {'EXIT', Parent, _} -> exit(normal) 
   after Interval ->
        ?MODULE:send_loop(State)
   end.
