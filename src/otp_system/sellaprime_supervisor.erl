%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2019 6:07 AM
%%%-------------------------------------------------------------------
-module(sellaprime_supervisor).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-behavior(supervisor).
-include_lib("../macros.hrl").

start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _arg = []),
  unlink(Pid).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
  my_alarm_handler:start(),

  % {RestartStrategy, MaxRestarts, InGivenAmountOfTime}
  {ok, {{one_for_one, 3, 10}, []}}.

%% starts a child w/ a unique tag for a given `Mod`
%% docs: http://erlang.org/doc/man/supervisor.html#start_child-2
-spec add(Mod::atom()) -> atom().
add(Mod) ->
  supervisor:start_child(
    sellaprime_supervisor, {
      round_robin:add(tag),
      {Mod, start_link, [round_robin:add(area_server)]},
      permanent,
      10000,
      worker,
      [Mod]}).