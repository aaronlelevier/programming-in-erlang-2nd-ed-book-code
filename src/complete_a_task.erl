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
  my_expected_finish_date/1,
  days_to_complete/1,
  will_finish_early/1,
  difference_in_days/2
]).

%%% config
%% expected number of total days to compete
expected_days_to_complete() -> 90.

%% total amount to do
total() -> 510.

%% amount to do per day
amount_per_day() -> 5.

%% add a number of days off to buffer the goal
buffer_days() -> 17.


%%% Functions

%% Returns the integer number of days until complete based
%% on the config
%% @param Current (int)
%% @return int
days_to_complete(Current) ->
  Total = total(),
  AmountPerDay = amount_per_day(),
  TotalDays = Total - Current,
  RawTotalDays = TotalDays div AmountPerDay,
  RawTotalDays + buffer_days().


%% Returns the expected finish date based on the config
%% @param Current (int)
%% @return (tuple) {Year, Month, Day}
my_expected_finish_date(Current) ->
  DaysToComplete = days_to_complete(Current),
  {Today, _} = calendar:universal_time(),
  calendar:gregorian_days_to_date(
    calendar:date_to_gregorian_days(Today) + DaysToComplete).


%% Returns a boolean value if we will finish early
%% @param Current (int)
%% @return bool
will_finish_early(Current) ->
  DaysToComplete = days_to_complete(Current),
  ExpectedDays = expected_days_to_complete(),
  DaysDiff = DaysToComplete - ExpectedDays,

  Answer = days_to_complete(Current) <
    expected_days_to_complete(),

  {Answer, {days_diff, DaysDiff}}.


%% Returns the integer date difference between dates
%% @param Date1 (tuple) {Year, Month, Day}
%% @param Date2 (tuple) {Year, Month, Day}
%% @return int
difference_in_days(Date1, Date2) ->
  Datetime1 = {Date1, {0,0,0}},
  Datetime2 = {Date2, {0,0,0}},
  {Days, _} = calendar:time_difference(Datetime1, Datetime2),
  Days.