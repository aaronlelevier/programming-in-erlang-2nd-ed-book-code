-module(funs).
-export([one_arg/0, mult_args/0, mult_clauses/0]).

%% simple fun with a single arg
one_arg() -> fun(X) -> X * 2 end.


%% can pass any number of args to a `fun`
mult_args() -> fun(A,B) -> A+B end.


%% can have multiple clauses
mult_clauses() ->
  fun({c,C}) -> {f, 32 + C*9/5};
     ({f,F}) -> {c, (F-32)*5/9}
  end.