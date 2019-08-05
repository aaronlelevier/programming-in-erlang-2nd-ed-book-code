%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2019 01:46
%%%-------------------------------------------------------------------
-module(after_t_seconds).
-author("aaron lelevier").

%% API
-export([sleep/1]).

sleep(T) ->
  receive
  after T ->
    ture
  end.