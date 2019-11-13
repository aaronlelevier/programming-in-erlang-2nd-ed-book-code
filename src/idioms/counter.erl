%%%-------------------------------------------------------------------
%%% @author alelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2019 12:22
%%%-------------------------------------------------------------------
-module(counter).
-author("alelevier").

%% API
-compile(export_all).
%%-export([bump/2, read/1]).

%% public
init() -> put(counter, 0).

incr() -> put(counter, read() + 1).

decr() -> put(counter, read() - 1).

add(N) -> put(counter, read() + N).

read() -> get(counter).

%% tests
test() ->
  init(),
  0 = read().