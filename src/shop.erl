-module(shop).
-export([cost/1, total/1]).

cost(oranges)	  -> 5;
cost(newspaper)	-> 8;
cost(pears)     -> 1;
cost(milk)      -> 4;
cost(apples)  	-> 2.

total([{What, N}|T]) -> cost(What)*N + total(T);
total([]) -> 0.
