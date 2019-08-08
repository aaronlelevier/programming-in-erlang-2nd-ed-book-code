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
-export([rpc/2, reply/0]).


%% self() is included here, so the receive statement only matches
%% on incoming messages that it sent, not all incoming messages
rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  end.


%% static reply to the process it gets messages from
reply() ->
  receive
    {From, Greeting} ->
      From ! {self(), "Reply: " ++ Greeting},
      reply()
  end.