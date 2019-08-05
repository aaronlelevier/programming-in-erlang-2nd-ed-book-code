%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2019 01:24
%%%-------------------------------------------------------------------
-module(flush).
-author("aaron lelevier").

%% API
-export([flush/0]).

flush() ->
  receive
    _Any ->
      flush()
  after 0 ->
    true
  end.
