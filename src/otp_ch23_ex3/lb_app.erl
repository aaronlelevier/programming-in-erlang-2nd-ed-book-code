%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2019 6:13 AM
%%%-------------------------------------------------------------------
-module(lb_app).
-author("aaron lelevier").
-behavior(application).
-include_lib("../macros.hrl").

-export([start/2, stop/1]).

%% test exports
-export([test/0]).

-define(SUPERVISOR, lb_supervisor).

start(_Type, Args) ->
  lb_supervisor:start_link(Args).

stop(_State) ->
  ok.

test() ->
  % init ETS table for round_robin rotation
  round_robin:init(),

  ?DEBUG("Initial Loaded..."),
  Loaded = application:loaded_applications(),
  ?DEBUG({loaded, Loaded}),

  ?DEBUG("Load lb..."),
  application:load(lb),
  Loaded2 = application:loaded_applications(),
  ?DEBUG({loaded, Loaded2}),

  ?DEBUG("App starting..."),
  application:start(lb),

  ?DEBUG("Initial worker count is 0"),
  0 = lb_server:worker_count(),

  ?DEBUG("lb_server status"),
  #{workers := #{}} = lb_server:get_status(),

  ?DEBUG("lb_supervisor should start w/ an empty workers map"),
  lb_supervisor:add(),

  ?DEBUG("Initial worker count is now 1"),
  1 = lb_server:worker_count(),

  ?DEBUG("lb_server status - now shows a worker present w/ 0 load"),
  #{workers := #{worker_server1 := 0}} = lb_server:get_status(),

  ?DEBUG("Add a 2nd worker"),
  lb_supervisor:add(),
  ?DEBUG({lb_server, worker_count, lb_server:worker_count()}),
  ?DEBUG({lb_server, status, lb_server:get_status()}).
