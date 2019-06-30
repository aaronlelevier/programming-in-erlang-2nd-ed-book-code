%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2019 09:36
%%%-------------------------------------------------------------------
-module(try_test).
-author("aaron").

%% API
-export([generate_exception/1, demo1/0, demo2/0, sqrt/1, check_busy/0]).

generate_exception(1) -> a;
generate_exception(2) -> {'EXIT', a};
generate_exception(3) -> throw(a);
generate_exception(4) -> error(a);
generate_exception(5) -> exit(a).


demo1() ->
  [catcher(I) || I <- [1,2,3,4,5]].


%% example of pattern matching of different errors
%% throw - is the ExceptionType
%% X = is a pattern match to what error message / data was thrown
catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, throw, X};
    error:X -> {N, caught, error, X};
    exit:X -> {N, caught, exit, X}
  end.


%% primitive "catch" example, before try..catch was introduced
demo2() ->
  [{I, (catch generate_exception(I))} || I <- [1,2,3,4,5]].


%% use exceptions to improve readability
sqrt(X) when X < 0 ->
  error({squareRootNegativeNotAllowed, X});
sqrt(X) ->
  math:sqrt(X).


%% use {ok, Value} or {error, Reason} as returns to communicate to caller
randomly_busy() ->
  Busy = rand:uniform() < 0.5,
  if Busy ->
      {error, busy};
    true ->
      {ok, not_busy}
  end.

check_busy() ->
  case randomly_busy() of
    {ok, not_busy} -> "let's go";
    {error, busy} -> "let's wait"
  end.