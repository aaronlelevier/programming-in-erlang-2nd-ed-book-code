%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2019 04:44
%%%-------------------------------------------------------------------
-module(ch4_exercises).
-author("aaron").

%% API
-export([list_to_tuple/1, my_time_func/0, my_date_string/0]).

%% converts a 3 item list to a tuple
%% TODO: not sure if this can be done to a list of unknown size?
list_to_tuple(L) ->
  3 = length(L),
  [A|T] = L,
  [B|T2] = T,
  [C|_] = T2,
  {A,B,C}.


%% initially tried with BIF now(), but that's deprecated!
my_time_func() ->
  Start = erlang:system_time(),

  Ret = lib_misc:for(1, 10, fun(X) -> X / 2 end),

  End = erlang:system_time(),
  TotalTime = End - Start,
  {Ret, TotalTime}.


my_date_string() ->
  {Year, Month, Day} = date(),
  {Hour, Minute, Second} = time(),
  io:fwrite(
    "Right now is: ~w ~w, ~w at ~w:~w:~w~n",
    [Month, Day, Year, Hour, Minute, Second]).
