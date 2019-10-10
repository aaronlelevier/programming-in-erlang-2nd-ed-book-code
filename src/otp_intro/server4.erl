%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% hot code swap and transactions
%%% @end
%%% Created : 10. Oct 2019 5:32 AM
%%%-------------------------------------------------------------------
-module(server4).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

start(Name, Mod) ->
  register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
  Name ! {self(), Request},
  receive
    {Name, crash} -> exit(rpc);
    {Name, ok, Response} -> Response;
    {Name, swap_code, Response} -> {swap_code, Response};
    Other -> {other, Other}
  end.

loop(Name, Mod, OldState) ->
  receive
    {From, {swap_code, NewCallbackMod}} ->
      From ! {Name, swap_code, NewCallbackMod},
      loop(Name, NewCallbackMod, OldState);
    {From, Request} ->
      try Mod:handle(Request, OldState) of
        {Response, NewState} ->
          From ! {Name, ok, Response},
          loop(Name, Mod, NewState)
      catch
        _:Why ->
          helpers:log_the_error(Name, Request, Why),
          From ! {Name, crash},
          loop(Name, Mod, OldState)
      end
  end.


%% NOTE: must change `name_server` to point to `server4` b/4 running test
test() ->
  true = server4:start(name_server, name_server1),
  ok = name_server1:add(joe, "at home"),
  ok = name_server1:add(helen, "at work"),
  {swap_code, new_name_server} = server4:swap_code(
    name_server, new_name_server),
  [joe, helen] = new_name_server:all_names(),
  ok.
