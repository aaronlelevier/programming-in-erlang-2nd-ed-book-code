%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2019 07:18
%%%-------------------------------------------------------------------
-module(ch5_maps).
-author("aaron").

%% API
-export([map_search_pred/2]).

%% returns true on first match of Pred tuple key/value to a
%% tuple key/value from the Map. False if no matches
map_search_pred(Map, Pred) ->
  [H|T] = maps:to_list(Map),
  IsMatch = Pred =:= H,
  if IsMatch ->
      true;
    T =:= [] ->
      false;
    true ->
      map_search_pred(maps:from_list(T), Pred)
    end.
