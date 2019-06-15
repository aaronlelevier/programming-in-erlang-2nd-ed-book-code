%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc Date arithmetic functions for calculating how long
%%% it will take to complete a task
%%%
%%% Example code showing the module working
%%%
%%% 1> c(complete_a_task).
%%% {ok,complete_a_task}
%%% 2> ExpectedDate = complete_a_task:my_expected_finish_date(63).
%%% {2019,9,29}
%%% 3> {Today, _} = calendar:universal_time().
%%% {{2019,6,15},{20,50,14}}
%%% 4> complete_a_task:difference_in_days(Today, ExpectedDate).
%%% 106
%%%
%%% @end
%%% Created : 15. Jun 2019 12:41
%%%-------------------------------------------------------------------
-module(complete_a_task).
-author("aaron").

%% API
-export([
  %% main entrypoint using config
  main/0,
  raw_expected_finish_date/2,
  raw_days_to_complete/1,
  raw_will_finish_early/2,
  number_of_days/2,
  today/0,
  number_of_weeks/2,
  days_to_complete/2,
  %% testing
  test/0
]).

%%% config
%% current completed
current() -> 63.
%% expected number of total days to compete
expected_days_to_complete() -> 90.
%% total amount to do
total() -> 510.
%% amount to do per day
amount_per_day() -> 5.
%% add a number of days off to buffer the goal, could
%% stand for vacation days, etc...
buffer_days() -> 17.
%% days off from goal per week
days_off_per_week() -> 1.

%% Tests
test() ->
  test_raw_expected_number_of_weeks(),
  test_raw_expected_finish_date(),
  test_raw_days_to_complete(),
  test_number_of_weeks_under_one(),
  test_number_of_weeks_over_one(),
  test_number_of_days(),
  {ok}.


test_raw_expected_number_of_weeks() ->
  Date = {2019, 6, 15},
  Current = 63,
  12 = raw_expected_number_of_weeks(Date, Current).


test_raw_expected_finish_date() ->
  Date = {2019, 6, 15},
  Current = 63,
  {2019, 9, 12} = raw_expected_finish_date(Date, Current).


test_raw_days_to_complete() ->
  Current = 63,
  89 = raw_days_to_complete(Current).


test_number_of_weeks_under_one() ->
  Date1 = {2019, 6, 15},
  Date2 = {2019, 6, 19},
  0 = number_of_weeks(Date1, Date2).


test_number_of_weeks_over_one() ->
  Date1 = {2019, 6, 15},
  Date2 = {2019, 6, 23},
  1 = number_of_weeks(Date1, Date2).


test_number_of_days() ->
  Date1 = {2019, 6, 15},
  Date2 = {2019, 6, 19},
  4 = number_of_days(Date1, Date2).


%%% Functions

main() ->
  Today = today(),
  Current = current(),
  DaysToComplete = days_to_complete(Today, Current),
  {
    {days, DaysToComplete},
    {weeks, DaysToComplete div 7},
    {completion_date, raw_expected_finish_date(Today, Current)}
  }.


%% @param Current (int)
days_to_complete(Today, Current) ->
  raw_days_to_complete(Current) + total_days_off(Today, Current).




%% Calculates the total days off if taking N number of
%% days offer per week
%% @param Current (int)
total_days_off_per_week(Today, Current) ->
  raw_expected_number_of_weeks(Today, Current) * days_off_per_week().


%% Total days off combining weekly days and buffer days
%% @param Current (int)
total_days_off(Today, Current) ->
  total_days_off_per_week(Today, Current) + buffer_days().


%% Returns a boolean value if we will finish early
%% @param Current (int)
%% @return bool
raw_will_finish_early(Date, Current) ->
  DaysToComplete = raw_days_to_complete(Current),
  ExpectedDate = raw_expected_finish_date(Date, Current),
  ExpectedDays =
  DaysDiff = DaysToComplete - ExpectedDays,
  Answer = DaysToComplete < ExpectedDays,
  {Answer, {days_diff, DaysDiff}}.


%% Returns the integer number of weeks to achieve the goal
%% based on the date and Current achieved
%% @param Current (int)
raw_expected_number_of_weeks(Date, Current) ->
  ExpectedDate = raw_expected_finish_date(Date, Current),
  number_of_weeks(Date, ExpectedDate).


%% Returns the expected finish date based on the config
%% @param Current (int)
%% @return (tuple) {Year, Month, Day}
raw_expected_finish_date(Date, Current) ->
  DaysToComplete = raw_days_to_complete(Current),
  calendar:gregorian_days_to_date(
    calendar:date_to_gregorian_days(Date) + DaysToComplete).


%% Returns the integer number of days until complete based
%% on the config
%% @param Current (int)
%% @return int
raw_days_to_complete(Current) ->
  Total = total(),
  TotalDays = Total - Current,
  TotalDays div amount_per_day().


%% Returns the integer number of weeks to achieve the goal
%% based on the dates
%% @param Date1 (tuple) {Year, Month, Day}
%% @param Date2 (tuple) {Year, Month, Day}
%% @param Current (int)
number_of_weeks(Date1, Date2) ->
  number_of_days(Date1, Date2) div 7.


%% Returns the integer date difference between dates
%% @param Date1 (tuple) {Year, Month, Day}
%% @param Date2 (tuple) {Year, Month, Day}
%% @return int
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
