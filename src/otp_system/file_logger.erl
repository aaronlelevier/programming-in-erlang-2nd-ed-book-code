%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% gen_event - docs: http://erlang.org/doc/man/gen_event.html
%%% otp design - events - docs: http://erlang.org/doc/design_principles/events.html
%%% @end
%%% Created : 21. Oct 2019 6:09 AM
%%%-------------------------------------------------------------------
-module(file_logger).
-author("aaron lelevier").
-behaviour(gen_event).
-include_lib("../macros.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, terminate/2, handle_call/2]).

%% tests
-export([file_logger_test/0]).

%% gen_event callbacks
-spec init(Args :: list()) -> {ok, State :: any()}.
init(File) ->
  {ok, Fd} = file:open(File, write),
  {ok, Fd}.

-spec handle_event(Event :: any(), State :: any()) -> {ok, State :: any()}.
handle_event(ErrorMsg, Fd) ->
  io:format(Fd, "{error, ~p}.~n", [ErrorMsg]),
  {ok, Fd}.

-spec handle_call(Args :: any(), State :: any()) -> {ok, Reply :: any(), State :: any()}.
handle_call(Args, State) ->
  ?DEBUG(Args),
  {ok, "Reply", State}.

-spec terminate(Args :: any(), State :: any()) -> ok.
terminate(_Args, Fd) ->
  file:close(Fd).

%%tests
file_logger_test() ->
  % starts Event Manager and registers it as Name "error_man"
  gen_event:start_link({local, error_man}),
  % `init`
  gen_event:add_handler(error_man, file_logger, ["./test.log"]),
  % `handle_event`
  gen_event:notify(error_man, no_reply),
  gen_event:notify(error_man, "some error"),
  % `handle_call`
  gen_event:call(error_man, file_logger, "yo").
  % `terminate`
  % gen_event:delete_handler(error_man, file_logger, []).