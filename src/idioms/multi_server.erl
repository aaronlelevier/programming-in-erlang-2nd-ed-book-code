%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2019 6:01 AM
%%%-------------------------------------------------------------------
-module(multi_server).
-author("aaron lelevier").
-compile(export_all).
-export([]).

start() -> spawn(fun() -> multi_server() end).

multi_server() ->
  receive
    {_Pid, {email, _From, _Subject, _Text} = Email} ->
      {ok, S} = file:open("mbox", [write, append]),
      io:format(S, "~p.~n", [Email]),
      file:close(S);
    {_Pid, {im, From, Text}} ->
      io:format("Msg (~s): ~s~n", [From, Text]);
    {Pid, {get, File}} ->
      Pid ! {self(), file:read_file(File)};
    Any ->
      io:format("multi server got: ~p~n", [Any])
  end,
  multi_server().