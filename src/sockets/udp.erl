%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2019 06:36
%%%-------------------------------------------------------------------
-module(udp).
-author("aaron lelevier").
-compile(export_all).
-export([]).

%% macros

-define(PORT, 4000).
-define(TIMEOUT, 1000).

% placeholder `init` for Makefile
init() -> ok.

%% server

server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  io:format("server opened socket:~p~n",[Socket]),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} = Msg ->
      io:format("server received:~p~n",[Msg]),
      N = binary_to_term(Bin),
      Fac = fac(N),
      % because `gen_udp:open` got `binary` as an option, the reply here is binary
      gen_udp:send(Socket, Host, Port, term_to_binary(Fac)),
      loop(Socket)
  end.

%% server that uses Ref

server2(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  io:format("server opened socket:~p~n",[Socket]),
  loop2(Socket).

loop2(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} = Msg ->
      io:format("server received:~p~n",[Msg]),
      {Ref, N} = binary_to_term(Bin),
      Fac = fac(N),
      % because `gen_udp:open` got `binary` as an option, the reply here is binary
      gen_udp:send(Socket, Host, Port, term_to_binary({Ref, Fac})),
      loop(Socket)
  end.

%% client

client(Request) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  io:format("client opened socket=~p~n",[Socket]),
  ok = gen_udp:send(Socket, "localhost", ?PORT, term_to_binary(Request)),
  Value = receive
            {udp, Socket, _Host, _Port, Bin} = Msg ->
              io:format("client received:~p~n",[Msg]),
              {ok, binary_to_term(Bin)}
          after ?TIMEOUT ->
            error
          end,
  gen_udp:close(Socket),
  Value.

%% client that uses Ref

client2(Request) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  io:format("client opened socket=~p~n",[Socket]),
  Ref = make_ref(),
  Request2 = term_to_binary({Ref, Request}),
  ok = gen_udp:send(Socket, "localhost", ?PORT, Request2),
  wait_for_ref(Socket, Ref).

wait_for_ref(Socket, Ref) ->
  receive
    {udp, Socket, _Host, _Port, Bin} = Msg ->
      io:format("client received:~p~n",[Msg]),
      case binary_to_term(Bin) of
        {Ref, Value} ->
          Value;
        {_SomeOtherRef, _Value} ->
          wait_for_ref(Socket, Ref)
      end
  after ?TIMEOUT ->
    error
  end.

%% functions

fac(1) -> 1;
fac(N) when N > 0 ->
  N * fac(N-1).

%% starts client/server for testing

%% client/server that doesn't use Ref
start(N) ->
  ServerPid = spawn(fun() -> server(?PORT) end),
  % let server start
  timer:sleep(?TIMEOUT),
  io:format("requesting factorial of:~p~n", [N]),

  Ret = client(N),

  io:format("received factorial of:~p~n", [Ret]),
  stop(ServerPid),
  ok.

%% client/server that uses Ref
start2(N) ->
  ServerPid = spawn(fun() -> server2(?PORT) end),
  % let server start
  timer:sleep(?TIMEOUT),
  io:format("requesting factorial of:~p~n", [N]),

  Ret = client2(N),

  io:format("received factorial of:~p~n", [Ret]),
  stop(ServerPid),
  ok.

stop(ServerPid) ->
  % Must stop Server or will get this error b/c address in use:
  % Error in process <0.119.0> with exit value:
  % {{badmatch,{error,eaddrinuse}},[{udp,server,1,[{file,"udp.erl"},{line,24}]}]}
  exit(ServerPid, done).
