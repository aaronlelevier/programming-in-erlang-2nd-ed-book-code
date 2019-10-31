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
-compile(export_all).
-export([]).
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

%% concat's atom and int for an atom name of a worker
-spec worker_name(Name::atom(), N::integer()) -> atom().
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
  ok.