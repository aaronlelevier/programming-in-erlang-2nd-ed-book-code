-module(shop).
-export([cost/1, total/1, sum/1, total2/1, list_comp/1, tests/0]).

cost(oranges)	-> 5;
cost(newspaper)	-> 8;
cost(pears)     -> 1;
cost(milk)      -> 4;
cost(apples)	-> 2.

total([{What, N}|T]) -> cost(What)*N + total(T);
total([]) -> 0.

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

total2(L) -> sum([shop:cost(What)*N || {What, N} <- L]).

list_comp(L) -> [X || {_, X} <- L].

tests() ->
  Buy = [{milk,1}, {oranges,4}, {apples,2}],
  10 = sum([2,3,5]),
  [1,4,2] = list_comp(Buy),
  28 = total2(Buy),
  ok.