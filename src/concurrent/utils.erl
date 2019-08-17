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
-export([start/0, rpc/2, reply/0, integer_to_atom/1, atom_to_integer/1,
  pid_name/1]).


start() -> ok.


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


integer_to_atom(N) ->
  list_to_atom(integer_to_list(N)).


atom_to_integer(A) ->
  list_to_integer(atom_to_list(A)).


pid_name(Pid) ->
  {registered_name, Name} = process_info(Pid, registered_name),
  Name.