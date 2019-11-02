%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2019 6:47 AM
%%%-------------------------------------------------------------------
-module(sellaprime_app).
-author("aaron lelevier").
-behavior(application).
-export([start/2, stop/1, test/0]).
-include_lib("../macros.hrl").


%% interface
start(_Type, StartArgs) ->
  sellaprime_supervisor:start_link(StartArgs).
stop(_State) -> ok.


%% tests
%% sellaprime_app:test().
test() ->
  % init ETS table for round_robin rotation
  round_robin:init(),

  ?DEBUG("Initial Loaded..."),
  Loaded = application:loaded_applications(),
  ?DEBUG({loaded, Loaded}),

  ?DEBUG("Load sellaprime..."),
  application:load(sellaprime),
  Loaded2 = application:loaded_applications(),
  ?DEBUG({loaded, Loaded2}),

  ?DEBUG("App starting..."),
  application:start(sellaprime),

  ?DEBUG("Supervisor adding child 1 ..."),
  ChildPid1 = sellaprime_supervisor:add(area_server),
  ?DEBUG({child_pid1, ChildPid1}),

  ?DEBUG("Supervisor adding child 2 ..."),
  {ok, ChildPid2} = sellaprime_supervisor:add(area_server),
  ?DEBUG({child_pid2, ChildPid2}),

  % make requests
  16 = area_server:area({square, 4}),
  25 = area_server:area({square, 5}),
  49 = area_server:area({square, 7}).
