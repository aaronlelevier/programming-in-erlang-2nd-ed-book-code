%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2019 6:55 AM
%%%-------------------------------------------------------------------
-module(round_robin).
-author("aaron lelevier").
-export([init/0, add/1, next/1, test/0, test2/0]).
-include_lib("../macros.hrl").

%% creates a new `ets` table
-spec init() -> atom().
init() ->
  ets:new(?MODULE, [set, named_table]).

%% takes a module name and returns and returns an atom of the
%% new worker name to use for this module ex: "area_server2"
-spec add(Name :: atom()) -> atom().
add(Name) ->
  Count = case ets:lookup(?MODULE, {Name, count}) of
            [] ->
              % if no count, insert initialize count
              0;
            [{{Name, count}, N}] ->
              N
          end,
  % increment count
  NewCount = Count + 1,
  ets:insert(?MODULE, {{Name, count}, NewCount}),
  % return the new worker name
  worker_name(Name, NewCount).

%% returns the current worker count based on the Name
-spec worker_count(Name :: atom()) -> integer().
worker_count(Name) ->
  case ets:lookup(?MODULE, {Name, count}) of
    [] -> -
    0;
    [{{Name, count}, N}] ->
      N
  end.

%% returns the next worker in the round robin to receive work
-spec next(Name :: atom()) -> {ok, NextName :: atom()} | {error, no_workers}.
next(Name) ->
  % first check if there are any workers
  case worker_count(Name) of
    0 ->
      {error, no_workers};
    Count ->
      next_worker(Name, Count)
  end.

%% returns the next worker in the rotation
-spec next_worker(Name :: atom(), Count :: integer()) -> {ok, NextName :: atom()}.
next_worker(Name, Count) ->
  % get the current worker from rotation
  Next = case ets:lookup(?MODULE, {Name, next}) of
           [] ->
             0;
           [{{Name, next}, N}] ->
             N
         end,

  % get next worker by N, if N is greater than the worker count
  % reset to 1, else N
  RealNext = if
               Next =:= Count ->
                 1;
               true ->
                 Next + 1
             end,

  % save current worker to rotation
  ets:insert(?MODULE, {{Name, next}, RealNext}),
  worker_name(Name, RealNext).

%% concat's atom and int for an atom name of a worker
-spec worker_name(Name :: atom(), N :: integer()) -> atom().
worker_name(Name, N) ->
  S = atom_to_list(Name),
  S2 = integer_to_list(N),
  list_to_atom(S ++ S2).

%% tests
%% run all tests
-spec test() -> ok.
test() ->
  % initialize ETS table
  init(),

  % worker name helper
  bob42 = worker_name(bob, 42),

  % add some workers
  area_server1 = add(area_server),
  area_server2 = add(area_server),
  foo1 = add(foo),

  % worker counts
  2 = worker_count(area_server),
  0 = worker_count(bizbazz),

  % worker doesn't exist
  {error, no_workers} = next(bizbazz),
  % get next worker from rotation
  area_server1 = next(area_server),
  area_server2 = next(area_server),
  % rotation resets because no workers after worker 2
  area_server1 = next(area_server),
  % other worker type has it's own rotation
  foo1 = next(foo),
  % only 1 worker in this rotation
  foo1 = next(foo),

  ok.

test2() ->
  init(),
  tag1 = add(tag).
