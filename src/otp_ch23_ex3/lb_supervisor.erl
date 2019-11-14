%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2019 6:08 AM
%%%-------------------------------------------------------------------
-module(lb_supervisor).
-author("aaron lelevier").
-behavior(supervisor).

%% interface exports
-export([start_link/1, start_in_shell_for_testing/0]).

%% supervisor exports
-export([init/1]).

%% interface
start_link(Args) ->
  supervisor:start_link({local, ?MACHINE}, ?MODULE, Args).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _arg = []),
  unlink(Pid).

%% supervisor callbacks
init([]) ->
  event_handler:start(),

  {ok, {{one_for_one, 3, 10},
    [{tag1,
      {lb_server, start_link, []},
      permanent,
      10000,
      worker,
      [lb_server]},
      {tag2,
        {worker_server, start_link, []},
        permanent,
        10000,
        worker,
        [worker_server]}
    ]}}.
