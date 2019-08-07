%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Template to start out with when writing concurrent code
%%% @end
%%% Created : 06. Aug 2019 19:54
%%%-------------------------------------------------------------------
-module(ctemplate).
-author("aaron lelevier").

%% API
-export([]).
-compile(export_all).

start() ->
  spawn(?MODULE, loop, []).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  end.

loop(X) ->
  receive
    Any -> io:format("Received:~p~n", [Any]),
      loop(X)
  end.