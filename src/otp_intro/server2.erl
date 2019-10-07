%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2019 6:07 AM
%%%-------------------------------------------------------------------
-module(server2).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

start(Name, Mod) ->
  register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

rpc(Name, Request) ->
  Name ! {self(), Request},
  receive
    {Name, crash} -> exit(rpc);
    {Name, ok, Response} -> Response;
    Other ->
      {other, Other}
  end.

loop(Name, Mod, OldState) ->
  receive
    {From, Request} ->
      try Mod:handle(Request, OldState) of
        {Response, NewState} ->
          From ! {Name, ok, Response},
          loop(Name, Mod, NewState)
      catch
        _:Why ->
          log_the_error(Name, Request, Why),
          From ! {Name, crash},
          loop(Name, Mod, OldState)
      end
  end.

log_the_error(Name, Request, Why) ->
  io:format("Server ~p request ~p ~n"
  "caused exception ~p~n", [Name, Request, Why]).

test() ->
  server2:start(name_server, name_server),
  ok = name_server:add(name, "Aaron"),
  {ok, "Aaron"} = name_server:find(name),
  ok.