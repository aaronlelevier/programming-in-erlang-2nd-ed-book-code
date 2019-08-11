%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Functions showing 3 methods communicating
%%% @end
%%% Created : 11. Aug 2019 06:59
%%%-------------------------------------------------------------------
-module(three).
-author("aaron lelevier").
-chapter(["12"]).
-desc(["p.187", "selective receive"]).
%% API
-export([]).
-compile(export_all).

start() -> ok.

%% prove that `rpc` waits for a response from a specific server

specific_loop() ->
  receive
  after 2000 ->
    receive
      {From, Request} ->
        From ! {self(), {server_specific, Request}}
    end,
    specific_loop()
  end.

any_loop(Pid) ->
  receive
  after 1000 ->
    Pid ! {ping, calendar:local_time()},
    any_loop(Pid)
  end.

%% example of "selective receive" - only receives responses
%% from the server it sent a request to
specific_rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  after 5000 ->
    io:fwrite("server_specific_rpc timeout")
  end.

any_rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    Response ->
      Response
  after 5000 ->
    io:fwrite("any_server_rpc timeout")
  end.