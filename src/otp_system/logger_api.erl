%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Test out Logger API
%%% Ref: http://erlang.org/doc/apps/kernel/logger_chapter.html
%%% @end
%%% Created : 18. Oct 2019 6:06 AM
%%%-------------------------------------------------------------------
-module(logger_api).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-include_lib("kernel/include/logger.hrl").

% placeholder `init` for Makefile
init() ->
  % how to set the log level
  % http://erlang.org/doc/apps/kernel/logger_chapter.html#example--add-a-handler-to-log-info-events-to-file
  logger:set_primary_config(level, info),
  start().

start() ->
  logger_functions(),
  logger_macros().

logger_functions() ->
  logger:error("something doesn't add up: ~p", [42]),
  logger:warning("this is a warning").

logger_macros() ->
  ?LOG_ERROR("some err"),
  ?LOG_INFO("some info").

add_handler() ->
  % config for logging to a file and log level is "info"
  Config = #{config => #{file => "./info.log"}, level => info},

  % creates a new handler named "myhandler" that uses the std library module
  % "logger_std_h" with config "Config"
  logger:add_handler(myhandler, logger_std_h, Config).