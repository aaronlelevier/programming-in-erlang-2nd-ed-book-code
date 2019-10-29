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

init() ->
  TableId = ets:new(?MODULE, [set, named_table]),
  TableId.