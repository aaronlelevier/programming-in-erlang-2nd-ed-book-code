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
  ?DEBUG("Initial Loaded..."),
  Loaded = application:loaded_applications(),
  ?DEBUG({loaded, Loaded}),

  ?DEBUG("Load lb..."),
  application:load(lb),
  Loaded2 = application:loaded_applications(),
  ?DEBUG({loaded, Loaded2}),

  ?DEBUG("App starting..."),
  application:start(lb).