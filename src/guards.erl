%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2019 04:45
%%%-------------------------------------------------------------------
-module(guards).
-author("aaron").

%% API
-export([get_max_fun/0, tests/0, get_get_list_fun/1, length_lt_4/1, when_and/1, when_or/1]).

tests() ->
  GetList = get_get_list_fun(3),
  [1,2,3] = GetList(),

  Max = get_max_fun(),
  9 = Max(1, 9),
  3 = Max(3, 2),
  4 = Max(4, 4),

  true = length_lt_4([1,2,3]),
  false = length_lt_4([1,2,3,4,5,6]),

  true = when_and(5),
  false = when_and(-1),

  true = when_or(11),

  ok.

%% simple fun
get_get_list_fun(N) -> fun() -> lists:seq(1,N) end.

%% leaving linted last value of X because it's part of
%% the func signature, so I don't want to make it an underscore
get_max_fun() -> fun(X, Y) when X > Y -> X; (X, Y) -> Y end.

%% predicate function - a function that returns a boolean
length_lt_4(X) -> erlang:length(X) < 4.

%% commas separating means "and"
when_and(X) when X > 0, X < 10 -> true;
when_and(_) -> false.

%% semi-colons me "or"
when_or(X) when X > 0; X < 10-> true;
when_or(_) -> false.
