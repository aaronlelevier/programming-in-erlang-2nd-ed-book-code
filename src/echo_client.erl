-module(echo_client).

-export([echo/2, awake/1, start_listener/1, listener/1]).

echo(Server, Msg) ->
  Server ! {self(), {echo, Msg}},
  receive
    {Server, ok} ->
      ok
  end.

start_listener(Server) -> spawn(echo_client, listener, [Server]).

listener(Server) ->
  receive
    {Server, {whine, Msg}} ->
      Msg
  end,
  listener(Server).

awake(Server) ->
  Server ! {self(), awake},
  receive
    {Server, ServerPid} ->
      ServerPid
  end.