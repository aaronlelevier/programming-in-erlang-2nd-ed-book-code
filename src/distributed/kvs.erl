%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2019 20:24
%%%-------------------------------------------------------------------
-module(kvs).
-author("aaron lelevier").
-desc("A Simple Name Server - p. 214").

-compile(export_all).

%% API
-export([]).


init() -> ok.


-spec start() -> boolean().

start() -> register(kvs, spawn(fun() -> loop() end)).


-spec store(Key, Value) -> boolean() when
  Key :: string(),
  Value :: any().

store(Key, Value) -> rpc({store, Key, Value}).


-spec lookup(Key) -> {ok, Value} | undefined when
  Key :: string(),
  Value :: any().

lookup(Key) -> rpc({lookup, Key}).


%% Private API

loop() ->
  receive
    {From, {store, Key, Value}} ->
      put(Key, {ok, Value}),
      From ! {kvs, true},
      loop();
    {From, {lookup, Key}} ->
      From ! {kvs, get(Key)},
      loop()
  end.

rpc(Q) ->
  kvs ! {self(), Q},
  receive
    {kvs, Response} ->
      Response
  end.

