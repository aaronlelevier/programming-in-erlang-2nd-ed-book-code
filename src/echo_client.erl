-module(echo_client).

-export([echo/2]).

echo(Server, Msg) ->
  Server ! {self(), {echo, Msg}},
  receive
    {Server, ok} ->
      ok
  end.