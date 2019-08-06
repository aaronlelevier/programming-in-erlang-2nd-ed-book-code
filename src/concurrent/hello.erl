%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2019 08:27
%%%-------------------------------------------------------------------
-module(hello).
-author("aaron lelevier").

%% API
-export([start/0, loop/0]).

start() -> spawn(hello, loop, []).

loop() ->
  receive
    {letters, Name} ->
      io:fwrite("Your name, ~p, has ~p letters(s)~n", [Name, length(Name)]),
      loop();
    Name ->
      io:fwrite("Hello ~p~n", [Name]),
      loop()
  end.