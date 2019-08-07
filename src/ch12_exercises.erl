%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Aug 2019 19:59
%%%-------------------------------------------------------------------
-module(ch12_exercises).
-author("aaron lelevier").

%% API
-export([init/0, loop/1, start_and_will_error/2, start_w_guard/2]).

%% use to spawn new a Process for `loop` to do it's work
init() ->
  {_, {_, Minutes, _}} = calendar:universal_time(),
  spawn(?MODULE, loop, [Minutes]).

%% registers a Process w/o a Guard, so if called more than once
%% will raise an exception
start_and_will_error(AnAtom, Fun) ->
    NewPid = spawn(Fun),
    register(AnAtom, NewPid),
    io:fwrite("start func registered: ~p~n", [NewPid]).

%% registers a Process using a Guard, so only 1 Process will be
%% registered with that atom
start_w_guard(AnAtom, Fun) ->
  Pid = whereis(AnAtom),
  if Pid == undefined ->
      NewPid = spawn(Fun),
      register(AnAtom, NewPid),
      io:fwrite("start func registered: ~p~n", [NewPid]);
    true ->
      io:fwrite("Registered:~p Requested:~p~n", [Pid, self()])
  end.

%% after 1<0 minutes has passed, the after/if block will be evaluated
loop(Minutes) ->
  receive
  after 1 ->
    {_, {_, Minutes2, _}} = calendar:universal_time(),
    if (Minutes+1) =:= Minutes2 ->
      start_w_guard(
          ch12,
          fun() -> io:fwrite("Registered: ~p~n", [self()]) end);
      true ->
        loop(Minutes)
    end
  end.

