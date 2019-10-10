%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% factorial server
%%% @end
%%% Created : 10. Oct 2019 6:05 AM
%%%-------------------------------------------------------------------
-module(my_fac_server).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

loop() ->
  receive
    {From, {fac, N}} ->
      From ! {self(), fac(N)},
      loop();
    {become, F} -> F()
  end.

fac(0) -> 1;
fac(N) -> N * fac(N - 1).