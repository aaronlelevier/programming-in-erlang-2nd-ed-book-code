%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2019 00:45
%%%-------------------------------------------------------------------
-module(processes).
-author("aaron lelevier").

%% API
-export([start/0, priority_receive/0, max/1, log_max/1]).
%% max(N)
%%   Create N processes then destroy them
%%   See how much time this takes

start() ->
  spawn(limb_misc, priority_receive, []).


priority_receive() ->
  receive
    {alarm, X} ->
      {alarm, X}
  after 0 ->
    receive
      Any -> Any
    end
  end.

%% returns a tuple of {total time, micro seconds} to start and
%% stop N number of processes
%%
%% Explanation of `runtime` vs `wall_clock`
%% http://erlang.org/pipermail/erlang-questions/2009-May/043698.html
max(N) ->
  statistics(runtime),
  statistics(wall_clock),
  L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  lists:foreach(fun(Pid) -> Pid ! die end, L),
  U1 = Time1 * 1000 / N,
  U2 = Time2 * 1000 / N,
  {U1, U2}.

%% logs the result of max/1 to stdout
log_max(N) ->
  % max processes allowed per Erlang
  Max = erlang:system_info(process_limit),
  io:format("Maximum allowed processes:~p~n",[Max]),
  % time per N processes
  {Time, MicroSec} = max(N),
  io:format("Process spawn time=~p (~p) microseconds~n",
    [Time, MicroSec]).

wait() ->
  receive
    die -> void end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
