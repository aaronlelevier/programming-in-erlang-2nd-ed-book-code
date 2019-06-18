%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jun 2019 18:34
%%%-------------------------------------------------------------------
-module(guards2).
-author("aaron").

%% API
-export([
  if_else_example/1,
  filter/2,
  odds_and_even2/1
]).

%% and/or evaluate both of their arguments

if_else_example(X) ->
  if
    X >= 1000 -> "too much";
    X >= 100 -> "let me think about it";
    true -> "ok"
  end.


%% case / switch example
%% Example usage
%% 64> guards2:filter(fun(X) -> is_integer(X) end, [1, 2.2, 0, 3.3, 4]).
%% [1,0,4]
filter(F, [H|T]) ->
  case F(H) of
    true -> [H|filter(F, T)];
    false -> filter(F, T)
  end;
filter(_, []) -> [].


%% Always add items to the head of a list. If order matters, then call `lists:reverse`
%% 65> L1 = [1,2,3].
%% [1,2,3]
%% 66> L2 = [4,5,6].
%% [4,5,6]
%% 67> [L1|L2].
%% [[1,2,3],4,5,6]
%% 68> [1,2,3|L2].
%% [1,2,3,4,5,6]
%% 69> lists:reverse([1,2,3|L2]).
%% [6,5,4,3,2,1]


%% Accumulators

%% separate a list by odds and evens by only traversing it once

odds_and_even2(L) ->
  odds_and_even_acc(L, [], []).

odds_and_even_acc([H|T], Odds, Evens) ->
  case (H rem 2) of
    1 -> odds_and_even_acc(T, [H|Odds], Evens);
    0 -> odds_and_even_acc(T, Odds, [H|Evens])
  end;
odds_and_even_acc([], Odds, Evens) ->
  %% initial ordering is reversed because on the pattern matching
  %% function above we're appended by the Head, so call `lists:reverse`
  %% to restore the order
  {lists:reverse(Odds), lists:reverse(Evens)}.