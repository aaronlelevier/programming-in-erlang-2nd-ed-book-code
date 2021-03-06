%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2019 23:57
%%%-------------------------------------------------------------------
-module(area_server1).
-author("aaron lelevier").

%% API
-export([start/0, area/2, rpc/2, loop/0]).

%% hides `spawn` from the user
start() -> spawn(area_server1, loop, []).

%% abstracts `rpc` and more accurately describes the function
area(Pid, What) ->
  rpc(Pid, What).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  end.

loop() ->
  receive
    {From, {rectangle, Width, Ht}} ->
      From ! {self(), Width * Ht},
      loop();
    {From, {circle, R}} ->
      From ! {self(), 3.14159 * R * R},
      loop();
    {From, Other} ->
      From ! {self(), {error, Other}},
      loop()
  end.
