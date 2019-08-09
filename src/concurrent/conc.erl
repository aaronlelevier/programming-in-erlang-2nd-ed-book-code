%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
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


call(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  after 5000 ->
    io:fwrite("no response~n"),
    false
  end.


reply_looper() ->
  receive
    {From, Greeting} ->
    From ! {self(), "Repling From:" ++ erlang:pid_to_list(self()) ++
      " To:" ++ erlang:pid_to_list(From) ++ " Reply:" ++ Greeting},
    reply_looper()
  end.