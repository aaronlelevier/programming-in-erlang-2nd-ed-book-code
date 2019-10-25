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
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
  sellaprime_supervisor:start_link(StartArgs).
stop(_State) -> ok.
