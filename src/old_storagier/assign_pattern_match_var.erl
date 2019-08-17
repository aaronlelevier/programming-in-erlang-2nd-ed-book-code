%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2019 06:42
%%%-------------------------------------------------------------------
-module(assign_pattern_match_var).
-author("aaron").

%% API
-export([temp_match/1, nested_temp_match/1]).


%% pattern match and assign to a temporary variable in the method signature
temp_match({tag, A, B}=Z) ->
  io:fwrite("A = ~p, B = ~p~n", [A, B]),
  io:fwrite("Z = ~p~n", [Z]).

nested_temp_match({tag, {nested, A}=Y, B}=Z) ->
  io:fwrite("A = ~p, B = ~p~n", [A, B]),
  io:fwrite("Y = ~p~n", [Y]),
  io:fwrite("Z = ~p~n", [Z]).
