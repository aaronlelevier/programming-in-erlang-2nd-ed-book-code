%%%-------------------------------------------------------------------
%%% @author alelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jun 2019 17:21
%%%-------------------------------------------------------------------
-module(mymaps).
-author("alelevier").

%% API
-export([new_map/0, add_new_key/1, update_existing_key/1, retrieve_value/1,
  count_characters/1]).

new_map() ->
  #{a => 1, b => 2}.

add_new_key(M) ->
  M#{c => 3}.

update_existing_key(M) ->
  M#{a := "new value"}.

retrieve_value(M) ->
  #{a := A} = M,
  A.


%% book example
count_characters(Str) ->
  count_characters(Str, #{}).

count_characters([H|T], X) ->
  Count = maps:get(H, X, 0),
  count_characters(T, maps:put(H, Count+1, X));
count_characters([], X) -> X.

