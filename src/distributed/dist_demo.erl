%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2019 06:02
%%%-------------------------------------------------------------------
-module(dist_demo).
-author("aaron lelevier").

-compile(export_all).

%% API
-export([]).


init() -> ok.

-spec start(Node) -> Pid when
  Node :: node(),
  Pid :: pid().

%% spawns a Process on Node, which is a separate Erlang machine
start(Node) ->
  spawn(Node, fun() -> loop() end).

-spec rpc(Pid, Mod, Func, Args) -> Response when
  Pid :: pid(),
  Mod :: atom(),
  Func :: atom(),
  Args :: [any()],
  Response :: any().

%% invokes loop and returns response
rpc(Pid, M, F, A) ->
  Pid ! {rpc, self(), M, F, A},
  receive
    {Pid, Response} ->
      Response
  end.

-spec loop() -> no_return().

%% calls `apply` on the current running Process and
%% send message with the result
loop() ->
  receive
    {rpc, Pid, M, F, A} ->
      Pid ! {self(), (catch apply(M, F, A))},
      loop()
  end.
