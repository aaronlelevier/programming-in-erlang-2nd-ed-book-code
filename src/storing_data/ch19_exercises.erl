%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2019 8:21 AM
%%%-------------------------------------------------------------------
-module(ch19_exercises).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

%% ch-19 ex-1 - iterate over all loaded modules and create
%% a ETS table where the key is {Func,Arity} and the value
%% is the module name
ex1() ->
  TableId = ets:new(bag, [bag]),
  L = [Mod || {Mod, _Path} <- code:all_loaded()],
  ex1_populate_table(L, TableId),
  ets:tab2list(TableId).

ex1_populate_table([], _TableId) ->
  ok;
ex1_populate_table(L, TableId) ->
  [Mod | T] = L,
  io:format("Mod:~p~n", [Mod]),
  Funcs = apply(Mod, module_info, [exports]),
  [ets:insert(TableId, {F, Mod}) || F <- Funcs],
  ex1_populate_table(T, TableId).