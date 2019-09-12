%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2019 6:00 AM
%%%-------------------------------------------------------------------
-module(ch17_exercises).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

%% ex-1 - convert the normal Bin response from `nano_get_url`
%% to the response format of Python's `request.get`

request(Host) ->
  Bin = tcp:nano_get_url(Host),
  Str = binary_to_list(Bin),
  StatusCode = string:slice(Str,9,3),
  {
    {status_code, list_to_integer(StatusCode)},
    {content, Bin}
  }.

test_request() ->
  Ret = request("www.google.com"),
  {200, Content} = Ret,
  io:format("~p~n", [Content]),

  Ret2 = request("www.commencal.co.uk"),
  {301, Content2} = Ret2,
  io:format("~p~n", [Content2]).

%% ex-2 - change the "simple TCP server" example that uses a Socket
%% to instead of sending a string, to send MFA and do apply(MFA)

start_nano_server() ->
  {ok, Listen} = listen(),
  {ok, Socket} = gen_tcp:accept(Listen), %% (7)
  gen_tcp:close(Listen), %% (8)
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n", [Bin]),
      MFA = binary_to_term(Bin), %% (9)
      io:format("Server (unpacked)  ~p~n", [MFA]),
      {Mod, Func, Args} = MFA,
      Reply = apply(Mod, Func, Args),
      io:format("Server replying = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)), %% (11)
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed~n")
  end.

listen() ->
  gen_tcp:listen(2345, [binary, {packet, 4}, %% (6)
    {reuseaddr, true},
    % passing the `{active, true}` Option here creates a nonblocking Server
    {active, true}]).

nano_client_eval({_Mod, _Func, _Args} = MFA) ->
  {ok, Socket} =
    gen_tcp:connect("localhost", 2345,
      [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary(MFA)),
  receive
    {tcp, Socket, Bin} ->
      io:format("Client received binary = ~p~n", [Bin]),
      Val = binary_to_term(Bin),
      io:format("Client result = ~p~n", [Val]),
      gen_tcp:close(Socket)
  end.

test_start_nano_server() ->
  spawn(fun() -> start_nano_server() end),
  Msg = {erlang, localtime, []},
  nano_client_eval(Msg).

%% ex-3 - same as ex-2 but use UDP

-define(PORT, 4000).
-define(TIMEOUT, 1000).

udp_client(Request) ->
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

udp_server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  io:format("server opened socket:~p~n",[Socket]),
  udp_loop(Socket).

udp_loop(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} = Msg ->
      io:format("server received binary:~p~n",[Msg]),
      MFA = binary_to_term(Bin),
      io:format("server decoded binary:~p~n",[MFA]),
      {Mod, Func, Args} = MFA,
      Result = apply(Mod, Func, Args),
      io:format("MFA Result:~p~n",[Result]),
      % because `gen_udp:open` got `binary` as an option, the reply here is binary
      gen_udp:send(Socket, Host, Port, term_to_binary(Result)),
      loop(Socket)
  after ?TIMEOUT ->
    timeout
  end.

test_udp_mfa() ->
  ServerPid = spawn(fun() -> udp_server(?PORT) end),
  MFA = {erlang, localtime, []},
  Ret = udp_client(MFA),
  % have to stop `udp_loop` or will get `{error,eaddrinuse}` b/c
  % udp server is still running, so Port is still in use
  exit(ServerPid, done),
  Ret.

%% ex-4 add a layer of cryptography to encode Bin b/4 sending and
%% decode when received by server
