-module(complete_a_task).
-author("Aaron Lelevier").

-export([
  %% prod
  completion_date/2,
  add_days/2,
  total_weeks_to_complete/1,
  total_days_to_complete/1,
  work_days_to_complete/1,
  days_off/1,
  number_of_weeks/2,
  number_of_days/2,
  today/0,
  %% testing
  test/0
]).


%% Config %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total() -> 510.
amount_per_day() -> 5.
buffer_days() -> 17.
days_off_per_week() -> 1.


%%% Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Returns the expected completion date
%% @param Date - erlang:date() - current date
%% @param Current - int - current page number
completion_date(Date, Current) ->
  Days = total_days_to_complete(Current),
  add_days(Date, Days).


add_days(Date, Days) ->
  calendar:gregorian_days_to_date(
    calendar:date_to_gregorian_days(Date) + Days).


total_weeks_to_complete(Current) ->
  total_days_to_complete(Current) div 7.


total_days_to_complete(Current) ->
  work_days_to_complete(Current) + buffer_days() + days_off(Current).


days_off(Current) ->
  (work_days_to_complete(Current) div 7) * days_off_per_week().


work_days_to_complete(Current) ->
  (total() - Current) div amount_per_day().


%% Returns the integer number of weeks to achieve the goal
number_of_weeks(Date1, Date2) ->
  number_of_days(Date1, Date2) div 7.


%% Returns the integer number days diff between 2 dates
number_of_days(Date1, Date2) ->
  Datetime1 = {Date1, {0,0,0}},
  Datetime2 = {Date2, {0,0,0}},
  {Days, _} = calendar:time_difference(Datetime1, Datetime2),
  Days.


%% Returns today's date
%% @return (tuple) {Year, Month, Day}
today() ->
  {Today, _} = calendar:universal_time(),
  Today.


%% Tests  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
  test_completion_date(),
  test_add_days(),
  test_total_weeks_to_complete(),
  test_total_days_to_complete(),
  test_days_off(),
  test_work_days_to_complete(),
  test_number_of_weeks(),
  test_number_of_days(),
  {ok}.


test_completion_date() ->
  Date = {2019, 6, 15},
  Current = 63,
  {2019, 10, 11} = completion_date(Date, Current).


test_add_days() ->
  Date = {2019, 6, 15},
  Days = 4,
  {2019, 6, 19} = add_days(Date, Days).


test_total_weeks_to_complete() ->
  16 = total_weeks_to_complete(63).


test_total_days_to_complete() ->
  118 = total_days_to_complete(63).


test_days_off() ->
  14 = days_off(10).


test_work_days_to_complete() ->
  89 = work_days_to_complete(63).


test_number_of_weeks() ->
  Date1 = {2019, 6, 15},
  Date2 = {2019, 6, 23},
  1 = number_of_weeks(Date1, Date2).


test_number_of_days() ->
  Date1 = {2019, 6, 15},
  Date2 = {2019, 6, 19},
  4 = number_of_days(Date1, Date2).