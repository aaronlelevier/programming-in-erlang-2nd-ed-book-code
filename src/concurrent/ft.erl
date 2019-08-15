%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Error Handling Book code pg. 207-8
%%% Sets up a Handler to monitor a process if it dies and call a Fun
%%% @end
%%% Created : 14. Aug 2019 07:01
%%%-------------------------------------------------------------------
-module(ft).
-author("aaron lelevier").
-chapter(["13"]).
-desc("ft - fault tolerance - book example #1").

%% API
-export([]).
-compile(export_all).


start() ->
  F = list_to_atom_fun(),

  % init process that converts a Message that's a `list()` to an `atom()`
  Pid = spawn(F),

  % setup handler
  set_on_exit_handler(Pid),

  % send message to Pid that raises an exception and triggers the handler
  Pid ! not_a_list.


set_on_exit_handler(Pid) ->
  on_exit(
    Pid,
    fun(Why) ->
      io:format("~p died with:~p~n", [Pid, Why])
    end).


%% on exit handler, that's a Monitor
on_exit(Pid, Fun) ->
  spawn(
    fun() ->
      Ref = monitor(process, Pid),
      receive
        {'DOWN', Ref, process, Pid, Why} ->
          Fun(Why)
      end
    end).


list_to_atom_fun() ->
  fun() ->
    receive
      X ->
        list_to_atom(X)
    end
  end.


%% attempt to exploit race condition between `register` and `on_exit`
%% call in `keep_alive` function
%% didn't work, but the output from `ft:spawn_two_kills` is not
%% idempotent. The # of processes started and killed each time
%% varies from 2-4 after doing about 5 tests

spawn_two_kills() ->
  spawn(fun() -> start_keep_alive_kill() end),
  spawn(fun() -> start_keep_alive_kill() end).


start_keep_alive_kill() ->
  Name = bob,
  keep_alive(Name, list_to_atom_fun()),
  for_kill(1, 5, Name),
  % check if process was killed by "for" loop or `keep_alive` worked?
  whereis(Name).


keep_alive(Name, Fun) ->
  register(Name, Pid = spawn(Fun)),
  io:fwrite("respawn: ~p~n", [Pid]),
  on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).


kill(Name) ->
  % send kill msg - Pid registered as "Name" converts a list()
  % to an atom() so this will kill the process
  Name ! 1.


for_kill(Max, Max, Name) -> kill(Name);
for_kill(Min, Max, Name) ->
  io:fwrite("for_kill~n"),
  kill(Name),
  for_kill(Min+1, Max, Name).