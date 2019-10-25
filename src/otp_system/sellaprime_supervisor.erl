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

start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _arg = []),
  unlink(Pid).

start_link(Args) ->
  supervisor:start_link({local, ?MACHINE}, ?MODULE, Args).

init([]) ->
  %%  gen_event:swap_handler(
  %%    alarm_handler,
  %%    {alarm_handler, swap},
  %%    {my_alarm_handler, xyz}),
  my_alarm_handler:start(),

  % {RestartStrategy, MaxRestarts, Time}
  % if MaxRestarts exceeded in given Time, Supervisor Process is killed
  {ok, {{one_for_one, 3, 10},
    [
      {tag1,
        {area_server, start_link, []}, % {Mod, Func, Args}
        permanent, % Restart
        10000, % Shutdown
        worker, % Type
        [area_server]}, % Modules - b/c the worker is a gen_server callback module
      {tag2,
        {prime_server, start_link, []},
        permanent,
        10000,
        worker,
        [area_server]}
    ]}}.
