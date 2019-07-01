%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2019 10:22
%%%-------------------------------------------------------------------
-module(ch7).
-author("aaron").

%% API
-export([converting_binaries/1]).


converting_binaries(Term) ->
  io:fwrite("Init Term: ~p~n", [Term]),

  Bin = term_to_binary(Term),
  io:fwrite("Term to Bin: ~p~n", [Bin]),

  Term1 = binary_to_term(Bin),
  io:fwrite("Bin to Term1: ~p~n", [Term1]),

  %% return a tagged tuple
  {bin, Bin}.