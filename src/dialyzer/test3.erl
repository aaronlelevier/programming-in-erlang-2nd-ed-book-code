%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2019 07:15
%%%-------------------------------------------------------------------
-module(test3).
-author("aaron").

%% API
-export([test/0, factorial/1]).


test() ->
  factorial(-5).

factorial(0) -> 0;
factorial(X) ->
  X * factorial(X-1).