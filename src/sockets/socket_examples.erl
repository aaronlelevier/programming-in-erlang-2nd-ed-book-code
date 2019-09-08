%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(socket_examples).
-compile(export_all).
-import(lists, [reverse/1]).

nano_get_url() ->
  nano_get_url("www.google.com").

nano_get_url(Host) ->
  {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]), %% (1)
  ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"), %% (2)
  receive_data(Socket, []).

receive_data(Socket, SoFar) ->
  receive
    {tcp, Socket, Bin} -> %% (3)
      receive_data(Socket, [Bin | SoFar]);
    {tcp_closed, Socket} -> %% (4)
      list_to_binary(reverse(SoFar)) %% (5)
  end.

nano_client_eval(Str) ->
  {ok, Socket} =
    gen_tcp:connect("localhost", 2345,
      [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary(Str)),
  receive
    {tcp, Socket, Bin} ->
      io:format("Client received binary = ~p~n", [Bin]),
      Val = binary_to_term(Bin),
      io:format("Client result = ~p~n", [Val]),
      gen_tcp:close(Socket)
  end.

%% aaron - start

%% empty init for makefile

init() -> ok.

listen() ->
  gen_tcp:listen(2345, [binary, {packet, 4}, %% (6)
    {reuseaddr, true},
    % passing the `{active, true}` Option here creates a nonblocking Server
    {active, true}]).

log_pid() ->
  io:fwrite("Pid:~p~n", [self()]).

%% sequential server example

start_seq_server() ->
  log_pid(),
  {ok, Listen} = listen(),
  io:fwrite("Listening~n"),
  seq_loop(Listen).

seq_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  io:fwrite("Socket accepted~n"),
  loop(Socket),
  seq_loop(Listen).

%% parallel server example

start_parallel_server() ->
  log_pid(),
  {ok, Listen} = listen(),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  log_pid(),
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

%% blocking server example

start_blocking_server() ->
  % passing the `{active, false}` Option here creates a blocking Server
  {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4}, {reuseaddr, true}, {active, false}]),
  io:fwrite("listening~n"),
  {ok, Socket} = gen_tcp:accept(Listen),
  io:fwrite("accepted~n"),
  blocking_loop(Socket).

blocking_loop(Socket) ->
  io:fwrite("passive_loop~n"),
  % number of bytes to receive from client at 1 time\

  % REVIEW: using `0` to accept any number of bytes here
  % docs: http://erlang.org/doc/man/gen_tcp.html#recv-2

  % based on this: http://erlang.org/doc/man/inet.html#setopts-2
  % I might have to use the Option `raw` in order set the number
  % of bytes that I want `gen_tcp:recv` to receive
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      io:format("Server received binary:~p size:~p~n", [Bin, size(Bin)]),
      Str = binary_to_term(Bin), %% (9)
      io:format("Server (unpacked)  ~p~n", [Str]),
      Reply = lib_misc:string2value(Str), %% (10)
      io:format("Server replying = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)),
      gen_tcp:close(Socket);
    {error, Reason} ->
      {error, Reason}
  end.

%% partial blocking server example (hybrid approach)

start_partial_blocking_server() ->
  % passing the `{active, false}` Option here creates a blocking Server
  {ok, Listen} = gen_tcp:listen(
    2345, [binary, {packet, 4}, {reuseaddr, true}, {active, once}]),
  io:fwrite("listening~n"),
  seq_partial_blocking_loop(Listen).

seq_partial_blocking_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  io:fwrite("accepted~n"),
  partial_blocking_loop(Socket),
  seq_partial_blocking_loop(Listen).

partial_blocking_loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n", [Bin]),
      Str = binary_to_term(Bin), %% (9)
      io:format("Server (unpacked)  ~p~n", [Str]),
      Reply = lib_misc:string2value(Str), %% (10)
      io:format("Server replying = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)), %% (11)

      % must call inet:setopts to reenable reception of the next msg
      inet:setopts(Socket, [{active, once}]),

      partial_blocking_loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed~n")
  end.

%% helper for testing a client request
make_example_client_request() ->
  nano_client_eval("1+2").

%% aaron - end

start_nano_server() ->
  {ok, Listen} = listen(),
  {ok, Socket} = gen_tcp:accept(Listen), %% (7)
  gen_tcp:close(Listen), %% (8)
  loop(Socket).
loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n", [Bin]),
      Str = binary_to_term(Bin), %% (9)
      io:format("Server (unpacked)  ~p~n", [Str]),
      Reply = lib_misc:string2value(Str), %% (10)
      io:format("Server replying = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)), %% (11)
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed~n")
  end.

error_test() ->
  spawn(fun() -> error_test_server() end),
  lib_misc:sleep(2000),
  {ok, Socket} = gen_tcp:connect("localhost", 4321, [binary, {packet, 2}]),
  io:format("connected to:~p~n", [Socket]),
  gen_tcp:send(Socket, <<"123">>),
  receive
    Any ->
      io:format("Any=~p~n", [Any])
  end.
error_test_server() ->
  {ok, Listen} = gen_tcp:listen(4321, [binary, {packet, 2}]),
  {ok, Socket} = gen_tcp:accept(Listen),
  error_test_server_loop(Socket).
error_test_server_loop(Socket) ->
  receive
    {tcp, Socket, Data} ->
      io:format("received:~p~n", [Data]),
      _ = atom_to_list(Data),
      error_test_server_loop(Socket)
  end.
