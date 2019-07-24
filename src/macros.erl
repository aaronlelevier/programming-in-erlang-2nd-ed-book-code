%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2019 06:12
%%%-------------------------------------------------------------------
-module(macros).
-author("aaron").

%% API
-export([
  file/0, module/0, line/0, module_string/0, call_macro1/1,
  debuggable_loop/1]).

%% built in macros

file() ->
  ?FILE.

module() ->
  ?MODULE.

line() ->
  ?LINE.

module_string() ->
  ?MODULE_STRING.

%% custom macro example
-define(macro1(X, Y), {a, X, Y}).

call_macro1(A) ->
  ?macro1(A + 10, b).

%% conditional Macro example for a debug print statement

-ifdef(debug).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n", [?MODULE, ?LINE, X])).
-else.
-define(DEBUG(X), void).
-endif.

%% if debug is on it prints the running total before the sum is returned
%% Total is determined by decrementing N each loop and adding to the sum
debuggable_loop(N) ->
  debuggable_loop(N, 0).

debuggable_loop(0, Total) -> Total;
debuggable_loop(N, Total) ->
  Val = Total + N,
  ?DEBUG(Val),
  debuggable_loop(N-1, Val).
