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

-export([init/1, handle_event/2, terminate/2, handle_call/2]).

init(_Args) ->
  {ok, []}.

handle_event(ErrorMsg, State) ->
  io:format("***Error*** ~p~n", [ErrorMsg]),
  {ok, State}.

terminate(_Args, _State) ->
  ok.

handle_call(_Request, State) ->
  {ok, "Reply", State}.