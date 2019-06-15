-module(echo_server).

-export([start/0, loop/0, whine/1]).

start() -> spawn(echo_server, loop, []).

loop() ->
  receive
    {Client, {echo, Msg}} ->
      io:format(Msg++"~n"),
      Client ! {self(), ok};

    {Client, awake} ->
      Server2 = spawn(echo_server, whine, [Client]),
      Client ! {self(), Server2}
  end,
  loop().

whine(Client) ->
  Client ! {self(), {whine, "Is anybody out there?"}},
  timer:sleep(1000),
  whine(Client).