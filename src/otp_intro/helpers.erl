%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2019 5:35 AM
%%%-------------------------------------------------------------------
-module(helpers).
-author("aaron lelevier").
-export([log_the_error/3]).

% placeholder `init` for Makefile
init() -> ok.

log_the_error(Name, Request, Why) ->
  io:format("Server ~p request ~p ~n"
  "caused exception ~p~n", [Name, Request, Why]).
