%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2019 6:48 AM
%%%-------------------------------------------------------------------
-module(application_template).
-author("aaron lelevier").
-behavior(application).
-export([start/2, stop/1]).

start(StartType, StartArgs) ->
  erlang:error(not_implemented).
stop(State) -> ok.


