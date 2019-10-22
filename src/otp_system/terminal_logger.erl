%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Oct 2019 6:07 AM
%%%-------------------------------------------------------------------
-module(terminal_logger).
-author("aaron lelevier").
-behaviour(gen_event).
-include_lib("kernel/include/logger.hrl").

-export([init/1, handle_event/2, terminate/2, handle_call/2, test/0]).

%% gen_event interface

init(N) ->
  ?LOG_INFO("initial state: ~p", [N]),
  {ok, N}.

handle_event({set_alarm, tooHot}, N) ->
  N2 = N + 1,
  ?LOG_INFO("set_alarm tooHot N:~p ", [N2]),
  {ok, N2};
handle_event({set_alarm, tooCold}, N) ->
  N2 = N - 1,
  ?LOG_INFO("set_alarm tooCold N:~p ", [N2]),
  {ok, N2};
handle_event({clear_alarm, tooHot}, N) ->
  N2 = 0,
  ?LOG_INFO("clear_alarm N:~p N2:~p", [N, N2]),
  {ok, N2};
handle_event(Event, N) ->
  ?LOG_INFO("Other:~p N:~p", [Event, N]),
  {ok, N}.

terminate(_Args, _N) ->
  ?LOG_INFO("MOD:~p terminating", [?MODULE]),
  ok.

handle_call(_Request, N) ->
  {ok, "Reply", N}.

%% tests

test() ->
  logger:set_primary_config(level, info),
  gen_event:start_link({local, alarm_handler}),
  gen_event:add_handler(alarm_handler, ?MODULE, 0),

  alarm_handler:set_alarm(tooHot),
  alarm_handler:set_alarm(tooHot),
  alarm_handler:set_alarm(tooCold),
  alarm_handler:clear_alarm(tooHot),

  gen_event:swap_handler(
    alarm_handler,
    {alarm_handler, swap},
    {my_alarm_handler, xyz}),

  gen_event:delete_handler(alarm_handler, ?MODULE, []),

  alarm_handler:set_alarm(tooHot),
  alarm_handler:set_alarm(tooHot),
  alarm_handler:set_alarm(tooCold),
  alarm_handler:clear_alarm(tooHot),

  gen_event:delete_handler(alarm_handler, my_alarm_handler, []).
