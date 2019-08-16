%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2019 20:26
%%%-------------------------------------------------------------------
-module(ch13_exercises).
-author("aaron lelevier").
-chapter(["13"]).

-compile(export_all).

%% API
-export([]).

start() -> ok.


%% ch-13 ex-1 - create a func `my_spawn(Mod, Func, Args)` that
%% when it dies logs how long it lived and says why it died

%% ch-13 ex-2 - also covered

start_ex1() ->
  % init process that converts a Message that's a `list()` to an `atom()`
  Pid = my_spawn(?MODULE, my_list_to_float, []),

  % elapsed time should be 2 seconds on the logged output
  timer:sleep(2000),

  % should cause Pid to die b/c it can only convert list() to float()
  Pid ! not_a_list.


my_spawn(Mod, Func, Args) ->
  Pid = spawn(Mod, Func, Args),

  % setup handler
  Start = erlang:universaltime(),
  set_on_exit_handler(Pid, Start),

  Pid.


my_list_to_float() ->
  receive
    X ->
      list_to_float(X)
  end.


set_on_exit_handler(Pid, Start) ->
  on_exit(
    Pid,
    Start,
    fun(Why, Time1, Time2) ->
      io:format(
        "~p died with:~p eplapsed:~p~n",
        [Pid, Why, datetime:elapsed_time(Time1, Time2)])
    end).


on_exit(Pid, Start, Fun) ->
  spawn(
    fun() ->
      Ref = monitor(process, Pid),
      receive
        {'DOWN', Ref, process, Pid, Why} ->
          Stop = erlang:universaltime(),
          Fun(Why, Start, Stop)
      end
    end).


%% ch-13 ex-3 - make a spawned process using the func signature
%% `my_spawn(Mod, Func, Args, Time)` that if still alive after
%% Time, will be killed

start_ex3(Time) ->
  % init process that converts a Message that's a `list()` to an `atom()`
  Pid = my_spawn(?MODULE, my_list_to_float, [], Time),

  % elapsed time should be 2 seconds on the logged output
  timer:sleep(2000),

  % should cause Pid to die b/c it can only convert list() to float()
  Pid ! not_a_list.

%% if still alive after `Time` has elapsed, func dies
my_spawn(Mod, Func, Args, Time) ->
  timer:kill_after(Time),
  my_spawn(Mod, Func, Args).
