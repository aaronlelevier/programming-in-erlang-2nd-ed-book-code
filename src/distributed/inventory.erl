%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc Usage:
%%%
%%% @end
%%% Created : 20. Aug 2019 06:28
%%%-------------------------------------------------------------------
-module(inventory).
-author("aaron lelevier").
-chapter(["14"]).
-desc("make rpc calls between two nodes, refer to `rpc` module").

-compile(export_all).

%% API
-export([]).

% placeholder `init` for Makefile
init() -> ok.

%% main loop that calls both client and server
%% NOTE: gandalf@ac - must be running, use `erl -sname gandalf`
%%  in a separate terminal
start() ->
  Pid = start_server(gandalf@ac),
  start_client(Pid),
  ok.

%% server

-spec start_server(Node) -> Pid when
  % name of server node to make rpc calls against
  Node :: node(),
  Pid :: pid().

start_server(Node) ->
  spawn(Node, fun() -> init_server() end).

init_server() ->
  init_inventory(),
  loop().

% initialize total inventory at 10
init_inventory() ->
  io:fwrite("init_inventory~n"),
  InitialTotal = 10,
  put(init_total, InitialTotal),
  put(total, InitialTotal),
  put(available, InitialTotal).

available() ->
  get(available).

total() ->
  get(total).

%% client

start_client(ServerPid) ->
  % how much total inventory is there
  % F (Function) = total
  % A (Args) = []
  Total = rpc(ServerPid, total, []),
  io:fwrite("Total:~p~n", [Total]),

  % do you have available inventory
  Available = rpc(ServerPid, available, []),
  io:fwrite("Available:~p~n", [Available]),

  % if available == total, then no inventory is in use
  Total = Available,

  % TODO: `reserve` could use a Map, where Key/Value = Pid/reserved
  % reserve some inventory

  % `available` should now be: total - reserved

  % reserve too much and get `error`

  % withdraw some ok

  % check `reserved`

  % withdraw too much and get error

  % withdraw all that I have `reserved`

  % new available == total

  ok.

%% rpc and loop

rpc(Pid, F, A) ->
  Pid ! {self(), {F, A}},
  receive
    {Pid, Response} ->
      Response;
    Response ->
      io:fwrite("no_match:~p~n", [Response]),
      {error, no_match, Response}
  after 1000 ->
    rpc_timeout
  end.


loop() ->
  receive
    {Pid, {F, A}} ->
      Pid ! {self(), apply(?MODULE, F, A)},
      loop();
    Request ->
      {error, no_match, Request}
  end.