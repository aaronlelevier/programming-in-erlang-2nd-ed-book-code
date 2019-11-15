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

%% debugging
-include_lib("../macros.hrl").
-compile(export_all).

%% interface
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _arg = []),
  unlink(Pid).

%% supervisor callbacks
init([]) ->
  event_handler:start(),

  {ok, {{one_for_one, 3, 10},
    % special tag name for `lb_server`
    [{lb_server_tag1,
      {lb_server, start_link, []},
      permanent,
      10000,
      worker,
      [lb_server]}
    ]}}.

%% starts a child w/ a unique tag for a given `Mod`
%% docs: http://erlang.org/doc/man/supervisor.html#start_child-2
-spec add() -> atom().
add() ->
  Mod = worker_server,
  ?DEBUG({"Adding worker", Mod}),

  supervisor:start_child(
    lb_supervisor, {
      round_robin:add(tag),
      {Mod, start_link, [round_robin:add(Mod)]},
      permanent,
      10000,
      worker,
      [Mod]}).