%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2019 07:14
%%%-------------------------------------------------------------------
-module(opaque_b).
-author("aaron").

%% API
-export([get_first_name/0, get_name/0]).

get_first_name() ->
  {First, _Last} = opaque_a:get_name(),
  First.

get_name() ->
  opaque_a:get_name().
