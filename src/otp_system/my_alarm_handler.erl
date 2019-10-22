%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 18. Oct 2019 6:06 AM
%%%-------------------------------------------------------------------
-module(my_alarm_handler).
-author("aaron lelevier").
-behaviour(gen_event).
-include_lib("kernel/include/logger.hrl").
-include_lib("../macros.hrl").

%% gen_server callbacks
-export([init0/0, init/1, handle_event/2, terminate/2, handle_call/2]).

%% makefile placeholder
init0() -> ok.

%% gen_server behaviour
init(Args) ->
	?DEBUG({"*** my_alarm_handler init", Args}),
	{ok, 0}.

handle_event({set_alarm, tooHot}, N) ->
	logger:error("*** tell the Engineer to turn on the fan"),
	{ok, N+1};
handle_event({set_alarm, tooCold}, N) ->
	logger:error("*** tell the Engineer to turn on the heat"),
	{ok, N-1};
handle_event({clear_alarm, tooHot}, N) ->
	logger:info("*** danger over, turn off the fan"),
	{ok, N}.

handle_call(_Request, N) ->
	Reply = N,
	{ok, Reply, N}.

terminate(_Args, _State) ->
	?LOG_INFO("MOD:~p terminating", [?MODULE]),
	ok.
