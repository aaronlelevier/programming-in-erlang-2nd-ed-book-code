%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Aug 2019 07:06
%%%-------------------------------------------------------------------
-module(stimer).
-author("aaron lelevier").

%% API
-export([]).
-compile(export_all).

%% book code

start(Time, Fun) -> spawn(fun() -> timer(Time, Fun) end).

cancel(Pid) -> Pid ! cancel.

timer(Time, Fun) ->
  receive
    cancel ->
      void
  after Time ->
    Fun()
  end.


%% implementing a timer - alarm version - p. 193


%% starts an `alarm` process and registers it under the name "alarm"
alarm(Time) ->
  Pid = spawn(fun() -> alarm_timer(Time) end),
  register(alarm, Pid),
  Pid.


%% actual timer of the alarm
alarm_timer(Time) ->
  receive
    cancel ->
      void
  after Time ->
    io:fwrite("beep beep~n")
  end.


%% pattern match, reply, and loop
loop_no_timeout() ->
  receive
    {From, done} ->
      From ! {ok, done},
      loop_no_timeout();
    {From, cancel} ->
      From ! {ok, false},
      loop_no_timeout()
  end.


%% will cancel the alarm and silence the `error:badarg` if the
%% alarm has already been canceled
cancel_alarm() ->
  try
    alarm ! cancel
  catch
    % will get a `error: bad argument` error if Pid has already stopped
    error:badarg ->
      io:fwrite("logging error~n"),
      error
  end.


%% receives 1 `{alarm, Message}` and logs to std out, then times out
receive_alarm() ->
  receive
    {alarm, Request} ->
      io:fwrite("Alarm:~p~n", [Request])
  after 0 ->
    false
  end.


%% receives 1 `Message` and logs to std out, then times out
receive_once() ->
  receive
    Request ->
      io:fwrite("Received:~p~n", [Request])
  after 0 ->
    false
  end.


%% example of guards on a function
non_negative_guard(N) when N < 0 -> throw({error, N});
non_negative_guard(N) when N =:= 0 -> N;
non_negative_guard(N) -> N.
