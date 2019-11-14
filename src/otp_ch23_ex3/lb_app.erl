%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2019 6:13 AM
%%%-------------------------------------------------------------------
-module(lb_app).
-author("aaron lelevier").
-behavior(application).
-export([start/2, stop/1]).

-define(SUPERVISOR, lb_supervisor).

start(_StartType, _StartArgs) ->
  case ?SUPERVISOR:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.
