%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Sep 2019 5:26 AM
%%%-------------------------------------------------------------------
-module(ex1).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

start() ->
  lists:foreach(fun test_ets/1, [set, ordered_set, bag, duplicate_bag]).

test_ets(Mode) ->
  TableId = ets:new(test, [Mode]),
  ets:insert(TableId, {a,1}),
  ets:insert(TableId, {b,2}),
  ets:insert(TableId, {a,1}),
  ets:insert(TableId, {a,3}),
  List = ets:tab2list(TableId),
  io:format("~-13w => ~p~n", [Mode, List]),
  ets:delete(TableId).

%% pattern matches on a list of 3 items
scan_trigrams([X,Y,Z] = L1, F, A) ->
  io:format("scan_trigrams 1:~p~n", [L1]),
  F([X,Y,Z], A);
%% pattern matches on a list of >3 items
scan_trigrams([X,Y,Z|T] = L1, F, A) ->
  io:format("scan_trigrams 2:~p~n", [L1]),
  A1 = F([X,Y,Z], A),
  scan_trigrams([Y,Z|T], F, A1);
%% pattern matches on a list of <3 items
scan_trigrams(_ = L1, _, A) ->
  io:format("scan_trigrams 3:~p~n", [L1]),
  A.

scan() ->
  Words = ["how", "many", "words", "are", "there"],
  F = fun(_, N) -> 1 + N  end,
  Acc = 0,
  scan_trigrams(Words, F, Acc).