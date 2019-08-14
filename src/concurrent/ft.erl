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

