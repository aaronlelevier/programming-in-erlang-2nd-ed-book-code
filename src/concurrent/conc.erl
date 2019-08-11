%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Functions showing 2 methods communicating
%%% @end
%%% Created : 09. Aug 2019 07:10
%%%-------------------------------------------------------------------
-module(conc).
-author("aaron lelevier").
-chapter(["2"]).
-vsn("v2").
-desc("2nd read of chapter").

%% API
-export([]).
-compile(export_all).

start() -> ok.


%% does a `call` communication
do_call_flow() ->
  Pid = spawn(?MODULE, reply_looper, []),
  call(Pid, "Hello").


%% send a message and wait for a reply
call(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    Response ->
      Response
  after 1000 ->
    io:fwrite("no response~n"),
    false
  end.


%% same as book `rpc` method
call_no_timeout(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    Response ->
      Response
  end.


receive_twice() ->
  receive
    Response ->
      io:fwrite("receive 1~n"),
      % this value will be lost
      Response
  end,
  receive
    Response2 ->
      io:fwrite("receive 2~n"),
      % only this value is returned
      Response2
  after 0 ->
    % only get in here if there's no match in the `receive`
    io:fwrite("after 1~n"),
    false
  end.


reply_looper() ->
  receive
    {From, Greeting} ->
      From ! {self(), "Repling From:" ++ erlang:pid_to_list(self()) ++
        " To:" ++ erlang:pid_to_list(From) ++ " Reply:" ++ Greeting},
      reply_looper()
  end.


%% priority_receive %%

start_priority_receive() ->
  Pid = spawn(?MODULE, priority_receive(), []),
  cast(Pid, "Hello").


priority_receive() ->
  receive
    {priority, Message} ->
      io:fwrite("priority:~p~n", [Message]),
      priority_receive();
    Message ->
      io:fwrite("normal:~p~n", [Message]),
      priority_receive()
  end.


priority_receive_server() ->
  receive
    {From, {priority, Message}} ->
      io:fwrite("priority:~p~n", [Message]),
      From ! Message,
      priority_receive_server();
    {From, Message} ->
      io:fwrite("normal:~p~n", [Message]),
      From ! Message,
      From ! "Reply two",
      priority_receive_server();
    Message ->
      io:fwrite("no match:~p~n", [Message]),
      Message,
      priority_receive_server()
  end.


%% send a message and don't wait for a reply
cast(Pid, Request) ->
  io:fwrite("sending: ~p~n", [Request]),
  Pid ! {self(), Request}.


%% receive all messages in the Pid's mailbox
flush() ->
  receive
    Response ->
      io:fwrite("received: ~p~n", [Response]),
      Response,
      flush()
  after 0 ->
    io:fwrite("no more messages~n"),
    true
  end.

%% flush messages and but don't log to stdout
flush_buffer() ->
  receive
    _Any ->
      flush_buffer()
  after 0 ->
    io:fwrite("no more messages~n"),
    true
  end.
