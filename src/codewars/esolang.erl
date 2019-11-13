%%%-------------------------------------------------------------------
%%% @author alelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2019 13:02
%%%-------------------------------------------------------------------
-module(esolang).
-author("alelevier").
-compile(export_all).
-include_lib("../macros.hrl").

%% API
-export([]).

my_first_interpreter(Code) ->
  L = parse(Code),
  lists:reverse(L).

parse(Code) ->
  parse(Code, 0, []).

parse([], _Val, Acc) -> Acc;
parse([H|T], Val, Acc) ->
  case H of
    $+ ->
      Val2 = Val+1,
      NewVal = case Val2 of
                 256 ->
                   0;
                 _ ->
                   Val2
               end,
      parse(T, NewVal, Acc);
    $. ->
      parse(T, Val, [Val|Acc]);
    _ ->
      parse(T, Val, Acc)
  end.

test() ->
  [67, 66, 65] = parse(".+.+.", 65, []).

format(L) ->
  io:format("~s~n", [L]).