%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% Goal: Compare the methods between Erlang `maps` and Python `dict`
%%%   and create a module of methods that are in `dict` but not `maps`
%%%
%%% maps: http://erlang.org/doc/man/maps.html
%%%
%%% dict: https://docs.python.org/3/library/stdtypes.html#typesmapping
%%%
%%% @end
%%% Created : 29. Jun 2019 06:32
%%%-------------------------------------------------------------------
-module(pydict).
-author("aaron").

%% API
-export([pop/2, pop/3]).


%% @doc Returns a tuple of the Map w/o the Key and the Value if Key in Map
%% If Key not in Map, throws an error
%% @returns {Map with key/value removed, Value that was removed}
%% @throws {badkey, Key}
%% @end
pop(Key, Map) ->
  IsKey = maps:is_key(Key, Map),
  if IsKey ->
      Map1 = maps:remove(Key, Map),
      Value = maps:get(Key, Map),
      {Map1, Value};
    true ->
      throw({badkey, Key})
  end.


%% Returns a tuple of the Map w/o the Key and the Key if Key in Map
%% If Key not in Map Returns Map and Default
pop(Key, Map, Default) ->
  try pop(Key, Map)
  catch
    throw:{badkey, Key} -> {Map, Default}
  end.
