%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2019 5:49 AM
%%%-------------------------------------------------------------------
-module(hhfuns).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

test() ->
  3 = add(fun one/0, fun two/0),
  ok.