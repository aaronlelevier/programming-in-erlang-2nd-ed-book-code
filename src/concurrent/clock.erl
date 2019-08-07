%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Aug 2019 19:48
%%%-------------------------------------------------------------------
-module(clock).
-author("aaron lelevier").

%% API
-export([start/2, stop/0]).

start(Time, Fun) ->
  register(clock, spawn(fun() -> tick(Time, Fun) end)).

stop() -> clock ! stop.

tick(Time, Fun) ->
  receive
    stop ->
      void
  after Time ->
    Fun(),
    tick(Time, Fun)
  end.