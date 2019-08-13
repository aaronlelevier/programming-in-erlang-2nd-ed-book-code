%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% ch-12 ex-3 - create a ring of N Process and pass M Messages
%%% around the ring and measure performance
%%% @end
%%% Created : 12. Aug 2019 20:14
%%%-------------------------------------------------------------------
-module(four).
-author("aaron lelevier").

%% API
-export([]).
-compile(export_all).


%% runs the main loop of the module, spawns 3 process and sends
%% 4 Messages to every process
send_messages() ->
  Pids = four:pid_list(3, 4),
  four:for("self() Msg", Pids).


%% N - # of processes to start
%% M - # of messages to send
pid_list(N, M) ->
  %% InitN - # of processes to start - initial value - saved
  %% b/c `send_to_next` needs this number for the round robin
  InitN = N,
  pid_list(N, InitN, M, []).

pid_list(0, _N2, _M, List) -> List;
pid_list(N, N2, M, List) ->
  Pid = spawn(?MODULE, loop, [N2, M]),
  Name = integer_to_atom(N),
  register(Name, Pid),
  pid_list(N - 1, N2, M, [Pid | List]).


%% logs incoming Message and sends it to next Process
%% N - # of processes
%% M - # of messages
loop(N, M) ->
  receive
    {From, Message} ->
      io:fwrite(
        "Pid:~p From:~p Name:~p Received:~p M:~p~n",
        [self(), From, pid_name(self()), Message, M]),
      if
        M > 1 ->
          send_to_next(N, "Pid Msg", self()),
          loop(N, M - 1);
        true -> loop_done
      end
  end.


%% pass a message to every process in a list
%% Msg - message
%% L - list of processes
for(Msg, L) ->
  Max = length(L),
  for(Max, Msg, L).

for(_Max, _Msg, []) -> for_done;
for(Max, Msg, [H | T]) ->
  send_to_next(Max, Msg, H),
  for(Max, Msg, T).


% if you are the Max, send it to the first else send Msg to next
send_to_next(Max, Msg, Pid) ->
  Name = pid_name(Pid),
  PidValue = atom_to_integer(Name),
  if
    PidValue == Max ->
      Next = whereis('1'),
      Next ! {self(), Msg};
    true ->
      Next = whereis(integer_to_atom(PidValue + 1)),
      Next ! {self(), Msg}
  end.


%% helpers

integer_to_atom(N) ->
  list_to_atom(integer_to_list(N)).


atom_to_integer(A) ->
  list_to_integer(atom_to_list(A)).


pid_name(Pid) ->
  {registered_name, Name} = process_info(Pid, registered_name),
  Name.