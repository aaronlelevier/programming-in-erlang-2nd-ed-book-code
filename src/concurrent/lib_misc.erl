%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2019 01:29
%%%-------------------------------------------------------------------
-module(lib_misc).
-author("aaron lelevier").

%% API
-export([
  start/0, sleep/1, sleep_forever/0, priority_receive/0,
  flush_mailbox/0, loop/0]).

start() ->
  spawn(limb_misc, priority_receive, []).

%% implements a `sleep` in Erlang
sleep(T) ->
  receive
  %% can set a timeout here after `T` milliseconds
  after T ->
    true
  end.

%% infinity - can be used as a timeout value. Not sure when to use this?
sleep_forever() ->
  receive
  after infinity ->
    true
  end.

%% TODO: always matching on the 2nd receive statement, need to learn how
%% to pre-load messages in the mailbox, then call this function and
%% the 1st receive should be evaluated
priority_receive() ->
  receive
    {alarm, X} ->
      io:fwrite("alarm..~n"),
      {alarm, X}
  after 0 ->
    receive
      Any ->
        io:fwrite("normal..~n"),
        Any,
        priority_receive()
      end
  end.

%% flushes all messages in the mailbox by evaluating the `receive`
%% statement one time, then returning
flush_mailbox() ->
  receive
    _Any ->
      flush_mailbox()
  after 0 ->
    true
  end.

%% TODO: need to better understand timeouts, this keeps hanging
loop() ->
  receive
    {From, {ask, Question}} ->
      From ! "You asked the question: " ++ Question,
      loop()
  after 10 ->
    "invalid request"
  end.
