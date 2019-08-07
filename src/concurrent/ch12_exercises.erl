%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% How to get the number of CPUs on my Mac (for ex2):
%%% $ sysctl hw.physicalcpu hw.logicalcpu
%%% @end
%%% Created : 06. Aug 2019 19:59
%%%-------------------------------------------------------------------
-module(ch12_exercises).
-author("aaron lelevier").

-import(processes, [max/1]).

%% API
-export([init/0, loop/1, start_and_will_error/2, start_w_guard/2,
  ex2/0]).

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


%% ex-2 spawn different amounts of process and plot creation time
ex2() ->
  % write header
  file:write_file("/tmp/plot", "Total,PerProcess\n"),
  Start = 1,
  % must be +1 the desired final log due to pattern match on final loop
  Max = 11,
  for(Start, Max).

for(N, N) -> ok;
for(I, N) ->
  io:fwrite("processing results for ~pk~n", [I]),
  {Total, PerProcess} = processes:max(I*1000),
  file:write_file("/tmp/plot", io_lib:fwrite("~p,~p\n",[Total, PerProcess]),
    [append]),
  for(I+1, N).
