%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2019 05:23
%%%-------------------------------------------------------------------
-module(a).
-author("aaron").

%% API
-export([start/1]).

start(Tag) ->
  spawn(fun() -> loop(Tag) end).

loop(Tag) ->
  sleep(),
  Val = b:x(),
  io:format("Vsn4: ~p; b:x() = ~p~n", [Tag, Val]),
  loop(Tag).

sleep() ->
  receive
  after 3000 -> true
  end.