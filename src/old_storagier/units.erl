%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jul 2019 06:25
%%%-------------------------------------------------------------------
-module(units).
-author("aaron").

-include_lib("records.hrl").

%% API
-export([
  % returns int
  available/1,
  % returns bool
  is_available/1,
  % print
  print_available/1,
  % guard
  guard_available/1,
  % test
  test/0
]).


%%%%%% macros %%%%%%


-ifdef(debug).
-define(DEBUG(Units, Avail), io:fwrite("Total: ~p InUnse: ~p Avail: ~p~n",
  [Units#units.total, Units#units.in_use, Avail])).
-else.
-define(DEBUG(Units, Avail), void).
-endif.


%%%%%% public %%%%%%

%% prints and returns the number of available units
%% throws error if Units counts are invalid
available(Units) ->
  print_available(Units),
  guard_available(Units),
  available_value(Units).


guard_available(Units) ->
  if
  % total can't be negative
    (Units#units.total < 0) or
      % in_use` can't be negative
    (Units#units.in_use < 0) or
      % in_use can't be greater than the total
    (Units#units.in_use > Units#units.total)
      ->
      throw(less_than_zero_error);

    true ->
      void
  end.


%% returns an integer of the available units
available_value(Units) ->
  Units#units.total - Units#units.in_use.


%% returns boolean if at least 1 unit is available
is_available(Units) ->
  print_available(Units),
  guard_available(Units),
  available_value(Units) > 0.


print_available(Units) ->
  Avail = available_value(Units),
  ?DEBUG(Units, Avail),
  Avail.


%%%%%% tests %%%%%%

test() ->
  ok = test_available_value(),
  ok = test_available(),
  ok = test_is_available(),
  ok = test_guard_available().


test_available() ->
  Units = #units{total = 4, in_use = 3},
  1 = units:available(Units),
  ok.


test_guard_available() ->
  % total less than in_use
  assert_less_than_zero(#units{total = 5, in_use = 50}),

  % total less than zero
  assert_less_than_zero(#units{total = -1}),

  % in_use less than zero
  assert_less_than_zero(#units{in_use = -1}),

  ok.


test_available_value() ->
  Units = #units{total = 5, in_use = 4},

  Ret = available_value(Units),

  1 = Ret,
  ok.


test_is_available() ->
  % available
  true = units:is_available(#units{total = 5, in_use = 4}),

  % not available
  false = units:is_available(#units{total = 5, in_use = 5}),

  ok.


%% test helpers

assert_less_than_zero(Units) ->
  try units:is_available(Units) of _ ->
    throw(failed_to_assert_less_than_zero)
  catch
    throw:less_than_zero_error -> void
  end.