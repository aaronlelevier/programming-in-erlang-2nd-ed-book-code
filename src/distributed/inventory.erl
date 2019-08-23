%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Usage:
%%%   init Server with: $ export NODE=gandalf && make
%%%   init Client with: $ export NODE=<client-name> && make
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
  io:fwrite("initializing inventory~n"),
  InitialTotal = 10,
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

  % reserve some inventory
  Amount = 4,
  log_inventory(ServerPid),
  {true, {available, NewCurAmount}} = rpc(
    ServerPid, reserve, [Amount]),
  io:fwrite("Reserved:~p NewCurAmount:~p~n", [Amount, NewCurAmount]),

  % `available` should now be: total - reserved
  log_inventory(ServerPid),
  NewCurAmount = rpc(ServerPid, available, []),
  io:fwrite("NewCurAmount:~p~n", [NewCurAmount]),
  NewCurAmount = 6,

  % reserve too much and get `error`
  Amount2 = 7,
  {false, {available, NewCurAmount2}} = rpc(
    ServerPid, reserve, [Amount2]),
  log_inventory(ServerPid),
  NewCurAmount2 = 6,

  % TODO: implement how to return of some inventory

  ok.


log_inventory(ServerPid) ->
  io:fwrite("Total:~p Available:~p~n",
    [rpc(ServerPid, total, []), rpc(ServerPid, available, [])]).


reserve(Amount) ->
  {IsAvailable, CurAmount} = check_available(Amount),
  io:fwrite(
    "IsAvailable:~p CurAmount:~p Amount:~p~n",
    [IsAvailable, CurAmount, Amount]),

  if IsAvailable == true ->
      NewCurAmount = CurAmount - Amount,
      put(self(), Amount),
      put(available, NewCurAmount),
      {true, {available, NewCurAmount}};
    true ->
      {false, {available, CurAmount}}
  end.


%% returns tuple of bool if request Amount is available
%% and the Current Amount of available inventory

-spec check_available(Amount) -> {boolean(), CurAmount} when
  Amount :: integer(),
  CurAmount :: integer().

check_available(Amount) ->
  X = get(available),
  io:fwrite("check_available:~p~n", [X]),

  if X >= Amount ->
      {true, X};
    true ->
      {false, X}
  end.


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


%% tests

% TODO: add tests
