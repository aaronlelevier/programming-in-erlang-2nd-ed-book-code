%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2019 05:22
%%%-------------------------------------------------------------------
-module(math_functions).
-author("aaron").

%% API
-export([even/1, odd/1, filter/2, split/1]).

even(X) ->
  X rem 2 =:= 0.


odd(X) ->
  X rem 2 =/= 0.


filter(F, [H|T]) ->
  case F(H) of
    true -> [H|filter(F, T)];
    false -> filter(F, T)
  end;
filter(_, []) -> [].


split(L) ->
  split_acc(L, [], []).

split_acc([H|T], Evens, Odds) ->
  if H rem 2 =:= 0 ->
    split_acc(T, [H|Evens], Odds);
    true -> split_acc(T, Evens, [H|Odds])
  end;
split_acc([], Evens, Odds) ->
  {Evens, Odds}.