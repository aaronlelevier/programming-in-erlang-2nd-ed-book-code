%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Sep 2019 5:55 AM
%%%-------------------------------------------------------------------
-module(ex2).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-chapter(["19"]).

-define(COUNT(), init_or_inc_count({?MODULE, ?LINE})).

% placeholder `init` for Makefile
start() ->
  init(),
  fac(5),

  % read a key that exists
  Key = ets:first(counts),
  Val = read(Key),
  io:format("Val:~p~n", [Val]),

  % read a key that does not exist
  Key2 = foo,
  Val2 = read(Key2),
  io:format("Val:~p~n", [Val2]),

  L = ets:tab2list(counts),
  io:format("~p~n~n", [L]).

init() ->
  ets:new(counts, [named_table]).

read(Key) ->
  ?COUNT(),
  case ets:lookup(counts, Key) of
    [] ->
      ?COUNT(),
      void;
    [{Key, Val}] ->
      ?COUNT(),
      Val
  end.

fac(0) -> 1;
fac(N) ->
  ?COUNT(),
  N * fac(N-1).

init_or_inc_count(Key) ->
  case ets:lookup(counts, Key) of
    [] ->
      ets:insert(counts, {Key, 1});
    [{Key, Count}] ->
      ets:insert(counts, {Key, Count+1});
    Other ->
      {error, Other}
  end.