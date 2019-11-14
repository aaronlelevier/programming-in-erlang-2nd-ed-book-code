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
-export([start/0, start_link/1, start_in_shell_for_testing/0]).

%% supervisor exports
-export([init/1]).

%% interface
start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_link(Args) ->
  supervisor:start_link({local, ?MACHINE}, ?MODULE, Args).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _arg = []),
  unlink(Pid).

%% supervisor callbacks
init([]) ->
  erlang:error(not_implemented).
