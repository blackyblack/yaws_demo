-module(stocks_app).
-include("yaws.hrl").

-behaviour(application).

%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% API

start() ->
  application:start(stocks, permanent).
    
stop() ->
  yaws:stop(),
  %% prepare to start application again
  application:unload(yaws),
  application:stop(stocks).
    
%% Application callbacks

%% start yaws as application and our local superivsor    
start(_StartType, _StartArgs) ->
  AppPath = filename:split(code:which(?MODULE)),
  RootPath = filename:join(lists:sublist(AppPath, length(AppPath) - 1)),
  YawsPath = lists:append(RootPath, "/../yaws/"),
  %% Profiling
  case application:get_env(stocks, debug_type) of
    {ok, profile} -> fprof:trace(start);
    _ -> ok
  end,
  
  c:cd(YawsPath),
  ok = application:load(yaws),
  ok = application:set_env(yaws, id, "server"),
  ok = application:set_env(yaws, config, YawsPath ++ "yaws.conf"),
  ok = application:start(yaws),

  %% start our server under supervisor
  stocks_sup:start_link().

stop(_State) ->
  ok.