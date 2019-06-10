-module(funs).
-export([
  one_arg/0, mult_args/0, mult_clauses/0,
  double_list/1, filter_even_orig/1, filter_even/1]).

%% simple fun with a single arg
one_arg() -> fun(X) -> X * 2 end.


%% can pass any number of args to a `fun`
mult_args() -> fun(A,B) -> A+B end.


%% can have multiple clauses
mult_clauses() ->
  fun({c,C}) -> {f, 32 + C*9/5};
     ({f,F}) -> {c, (F-32)*5/9}
  end.


%% ex. pg 54 - double all elements in a list
double_list(L) ->
  lists:map(fun(X) -> X * 2 end, L).


%% ex pg. 54 filter a list using `lists:filter` std lib
filter_even_orig(L) ->
  lists:filter(fun(X) -> X rem 2 =:= 0 end, L).


%% NOTE: the above actions are called "list-at-a-time" operations


%% implement my own "filter even" w/o std lib
filter_even([H|T]) ->
  if
    H rem 2 =:= 0 ->
      [H|filter_even(T)];
    true ->
      filter_even(T)
  end;
filter_even([]) -> [].