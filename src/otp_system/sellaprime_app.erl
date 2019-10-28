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
test() ->
  ?DEBUG("Initial Loaded..."),
  Loaded = application:loaded_applications(),
  ?DEBUG({loaded, Loaded}),

  ?DEBUG("Load sellaprime..."),
  application:load(sellaprime),
  Loaded2 = application:loaded_applications(),
  ?DEBUG({loaded, Loaded2}),

  ?DEBUG("Starting..."),
  application:start(sellaprime).
