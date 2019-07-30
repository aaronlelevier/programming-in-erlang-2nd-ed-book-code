%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2019 06:26
%%%-------------------------------------------------------------------
-module(types1).
-author("aaron").

-chapter(["9"]).

%% API
-export([f1/1, f2/1, f3/1]).


f1({H,M,S}) ->
  (H*60+M)*60+S.


f2({H,M,S}) when is_integer(H) ->
  (H*60+M)*60+S.


f3({H,M,S}) ->
  print(H,M,S),
  (H*60+M)*60+S.


print(H,M,S) ->
  Str = integer_to_list(H) ++ ":" ++ integer_to_list(M) ++ ":" ++
    integer_to_list(S),
  io:fwrite("~p~n", [Str]).