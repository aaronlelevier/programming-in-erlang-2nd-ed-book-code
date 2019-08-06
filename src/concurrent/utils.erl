%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2019 18:33
%%%-------------------------------------------------------------------
-module(utils).
-author("aaron lelevier").

%% API
-export([rpc/2]).


rpc(Pid, Request) ->
  %% self() is included here, so the receive statement only matches
  %% on incoming messages that it sent, not all incoming messages
  Pid ! {self(), Request},
  receive
    Response ->
      Response
  end.
