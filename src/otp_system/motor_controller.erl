%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 6:02 AM
%%%-------------------------------------------------------------------
-module(motor_controller).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

add_event_handler() ->
  event_handler:add_handler(errors, fun controller/1).

controller(too_hot) ->
  io:format("turn off motor~n");
controller(X) ->
  io:format("~w event ignored:~p~n", [?MODULE, X]).
