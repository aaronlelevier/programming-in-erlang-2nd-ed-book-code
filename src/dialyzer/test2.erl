%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2019 07:13
%%%-------------------------------------------------------------------
-module(test2).
-author("aaron").

%% API
-export([f1/0]).

%% dialyzer detects incorrect arg type to a BIF

f1() ->
  list_to_tuple({a,b,c}).

