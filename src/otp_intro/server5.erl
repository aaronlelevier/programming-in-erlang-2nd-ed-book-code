%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% server that does nothing until it becomes a certain type of server
%%% @end
%%% Created : 10. Oct 2019 6:03 AM
%%%-------------------------------------------------------------------
-module(server5).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

start() -> spawn(fun() -> wait() end).

wait() ->
  receive
    {become, F} -> F()
  end.

rpc(Pid, Q) ->
  Pid ! {self(), Q},
  receive
    {Pid, Reply} -> Reply
  end.