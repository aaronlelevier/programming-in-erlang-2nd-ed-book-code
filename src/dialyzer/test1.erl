%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2019 07:07
%%%-------------------------------------------------------------------
-module(test1).
-author("aaron").

%% API
-export([f1/0]).

f1() ->
  X = now(),
  seconds(X).

%% fails dialyzer b/c `now()` above returns a 3 item tuple
seconds(X) ->
  {_Year, _Month, _Day, Hour, Min, Seconds} = X,
  (Hour * 60 + Min) * 60 + Seconds.