%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2019 5:46 AM
%%%-------------------------------------------------------------------
-module(server3).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

start(Name, Mod) ->
  register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) ->
  rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
  Name ! {self(), Request},
  receive
    {Name, Response} ->
      Response
  end.

loop(Name, Mod, OldState) ->
  receive
    {From, {swap_code, NewCallbackMod}} ->
      From ! {Name, ack},
      loop(Name, NewCallbackMod, OldState);
    {From, Request} ->
      {Response, NewState} = Mod:handle(Request, OldState),
      From ! {Name, Response},
      loop(Name, Mod, NewState)
  end.


test() ->
  true = server3:start(name_server, name_server1),
  ok = name_server1:add(joe, "at home"),
  ok = name_server1:add(helen, "at work"),
  ack = server3:swap_code(name_server, new_name_server),
  [joe, helen] = new_name_server:all_names(),
  ok.
