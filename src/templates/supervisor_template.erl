%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2019 5:45 AM
%%%-------------------------------------------------------------------
-module(supervisor_template).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-behavior(supervisor).

%% interface
start() ->
  spawn(fun() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _arg = []),
  unlink(Pid).

start_link(Args) ->
  supervisor:start_link({local, ?MACHINE}, ?MODULE, Args).

%% supervisor callbacks
init([]) ->
  erlang:error(not_implemented).