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


%% ex-4 - func that every 5 seconds says "I'm still alive" and func that
%% monitors and restarts the 1st process if it dies

%% moves timeout configuration outside of `log_i_am_still_alive`
%% can also no be used elsewhere
-define(TIMEOUT, 5000).

%% start main loop for ex-4
start_ex4() ->
  spawn(?MODULE, supervisor, []).


%% watches our "worker" and if it dies, restarts it
supervisor() ->
  {Pid, Ref} = respawn(),
  receive
    {'DOWN', Ref, process, Pid, Why} ->
      io:fwrite("worker Pid:~p died because:~p~n", [Pid, Why]),
      io:fwrite("re-spawning~n"),
      supervisor()
  end.


respawn() ->
  {Pid, Ref} = spawn_monitor(?MODULE, log_i_am_still_alive, []),
  % don't need to de-register the previous "worker" Pid
  % just works. If previous Pid dies it's auto de-registered
  register(worker, Pid),
  {Pid, Ref}.


log_i_am_still_alive() ->
    io:fwrite(
      "~p time:~p I'm still alive~n", [self(), erlang:universaltime()]),
    timer:sleep(?TIMEOUT),
    log_i_am_still_alive().


%% ex-5 - spawn several worker processes, and if any of them die
%% restart them
%% works by registering each worker under an "integer" Name
%% so we can kill by Name, and a new worker will be spawned
%% by incrementing the "integer" Name and then register

%% TODO: this SO Answer on how to do this would be more idiomatic
%% https://stackoverflow.com/questions/44250378/erlang-monitor-multiple-processes

-define(WORKER_COUNT, 3).

start_ex5() ->
  spawn(?MODULE, supervisor2, []).

supervisor2() ->
  init_spawn(),
  loop_single(?WORKER_COUNT).

init_spawn() ->
  init_spawn(0, ?WORKER_COUNT).

init_spawn(Max, Max) -> done;
init_spawn(Min, Max) ->
  io:fwrite("int_spawn Min:~p~n", [Min]),
  spawn_single(Min),
  init_spawn(Min+1, Max).

loop_single(Count) ->
  receive
    {'DOWN', _Ref, process, Pid, Why} ->
      io:fwrite("worker Pid:~p died because:~p~n", [Pid, Why]),
      io:fwrite("re-spawning~n"),
      % retrieve registered name of process that died, so we can
      % re-register under the same name
      spawn_single(Count),
      loop_single(Count+1)
  end.

spawn_single(Min) ->
  {Pid, _Ref} = spawn_monitor(?MODULE, log_i_am_still_alive, []),
  Name = utils:integer_to_atom(Min),
  register(Name, Pid),
  io:fwrite("spawn_single spawned:~p Name:~p~n", [Pid, Name]),
  Pid.


%% ch-13 ex-6 - have a Supervisor monitor workers, and if one dies,
%% kill all workers and re-spawn

start_ex6() ->
  spawn(?MODULE, supervisor3, []).

supervisor3() ->
  % register Supervisor as a system process
  process_flag(trap_exit, true),
  % start workers
  LastWorkerPid = spawn_linked_workers(),
  io:fwrite(
    "LastWorkerPid:~p, Name:~p~n",
    [LastWorkerPid, utils:pid_name(LastWorkerPid)]),
  loop_watch_linked_workers(?WORKER_COUNT, LastWorkerPid).

spawn_linked_workers() ->
  % ? does "name explaining VAR work here ?
  spawn_linked_workers(0, ?WORKER_COUNT, _PrevWorkerPid = undefined).

spawn_linked_workers(Max, Max, LastWorkerPid) -> LastWorkerPid;
spawn_linked_workers(Min, Max, PrevWorkerPid) ->
  Pid = spawn_linked_worker(Min, PrevWorkerPid),
  spawn_linked_workers(Min+1, Max, Pid).

spawn_linked_worker(Min, PrevWorkerPid) ->
  io:fwrite("PrevWorkerPid:~p~n", [PrevWorkerPid]),
  Pid = spawn_link(?MODULE, link_and_log, [PrevWorkerPid]),
  Name = utils:integer_to_atom(Min),
  register(Name, Pid),
  io:fwrite("spawn_single spawned:~p Name:~p~n", [Pid, Name]),
  Pid.

loop_watch_linked_workers(Count, LastWorkerPid) ->
  receive
    {'EXIT', Pid, Why} ->
      io:fwrite("worker Pid:~p died because:~p~n", [Pid, Why]),
      io:fwrite("re-spawning~n"),
      % retrieve registered name of process that died, so we can
      % re-register under the same name
      NewWorkerPid = spawn_linked_worker(Count, LastWorkerPid),
      loop_watch_linked_workers(Count+1, NewWorkerPid)
  end.

%% does "maybe link" as a single action to the PrevWorkerPid, then
%% does an infinite loop every ?TIMEOUT that itself is still alive
link_and_log(PrevWorkerPid) ->
  maybe_link(PrevWorkerPid),
  log_i_am_still_alive().

maybe_link(Pid) ->
  try
    link(Pid)
  catch
    % Pid trying to link has already died case
    error:noproc  ->
      io:fwrite("noproc:~p~n", [Pid]);
    % Pid == undefined case
    error:badarg ->
      io:fwrite("badarg:~p~n", [Pid])
  end.
