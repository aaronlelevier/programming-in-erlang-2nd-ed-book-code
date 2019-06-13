-module(echo_server).

-export([start/0, loop/0]).

start() -> spawn(echo_server, loop, []).

loop() ->
  receive
    {Client, {echo, Msg}} ->
      io:format(Msg++"~n"),
      Client ! {self(), ok}
  end,
  loop().