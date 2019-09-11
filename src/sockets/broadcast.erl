%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2019 6:07 AM
%%%-------------------------------------------------------------------
-module(broadcast).
-author("aaron lelevier").
-compile(export_all).
-export([]).

-define(BROADCAST_PORT, 5010).
-define(LISTEN_PORT, 6000).

% placeholder `init` for Makefile
init() -> ok.

start() ->
  LoopPid = spawn(fun() -> listen() end),
  IoList = "I am bob",
  Ret = send(IoList),
  {Ret, LoopPid}.

send(IoList) ->
  case inet:ifget("en0", [broadaddr]) of
    {ok, [{broadaddr, Ip}]} ->
      io:format("broadaddr for Ip:~p~n", [Ip]),
      {ok, Socket} = gen_udp:open(?BROADCAST_PORT, [{broadcast, true}]),
      gen_udp:send(Socket, Ip, ?LISTEN_PORT, IoList),
      gen_udp:close(Socket);
    _ ->
      io:format("Bad interface name or broadcasting not supported")
  end.

listen() ->
  {ok, _} = gen_udp:open(?LISTEN_PORT),
  loop().

loop() ->
  receive
    Any ->
      io:format("Received:~p~n", [Any]),
      loop()
  end.
