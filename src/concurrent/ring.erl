%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2019 18:59
%%%-------------------------------------------------------------------
-module(ring).
-author("aaron lelevier").

%% API
-export([start/0, init/1, loop/1, start_chat/1, chat/1,
  initchat/0, receive_last_message/0]).

start() -> ok.


init(N) ->
  Id = integer_to_atom(N),

  Fun = fun(N2) ->
    Pid = spawn(?MODULE, loop, [N2]),
    io:fwrite("Pid: ~p~n", [Pid]),
    register(Id, Pid) end,

  for(1, N, Fun).


integer_to_atom(N) ->
  list_to_atom(integer_to_list(N)).


for(N, N, F) ->
  io:fwrite("FN: ~p~n", [N]),
  F(N);
for(I, N, F) ->
  io:fwrite("FI: ~p~n", [I]),
  F(I),
  for(I+1, N, F).


loop(N) ->
  receive
  after 0 ->
    io:fwrite("id: ~p~n", [N])
  end.


%% create `chat` process
start_chat(N) ->
  spawn(?MODULE, chat, [N]).


%% 2 Processes that chat. They should log the Pid and the Msg #
%% and exit after N times.
chat(Max) ->
  receive
    {From, Max} when is_integer(Max) ->
      io:fwrite("Msg From: ~p Max: ~p Done!~n", [From, Max]),
      From ! {self(), Max};
    {From, N} when is_integer(N) ->
      io:fwrite("Msg From: ~p N: ~p~n", [From, N]),
      From ! {self(), N},
      chat(Max);
    {From, Other} ->
      From ! {self(), {error, Other}},
      chat(Max)
  end.

initchat() ->
  N = 3,
  Pid1 = start_chat(N),
  Pid2 = start_chat(N),
  io:fwrite("Pid1:~p Pid2:~p~n", [Pid1, Pid2]),

  % I is the Request for `rpc`
  Fun = fun(I) -> utils:rpc(Pid1, I) end,

  for(0, N, Fun).


%% returns the last Message from the Mailbox or `done`
receive_last_message() ->
  receive R -> R after 0 -> done end.
