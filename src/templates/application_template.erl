%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2019 6:48 AM
%%%-------------------------------------------------------------------
-module(application_template).
-author("aaron lelevier").
-behavior(application).

%% application exports
-export([start/2, stop/1]).

%% TODO: must be defined
-define(SUPERVISOR, tbd).

%% application callbacks
start(_StartType, _StartArgs) ->
  case ?SUPERVISOR:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

stop(_State) ->
  ok.
