%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2019 6:01 AM
%%%-------------------------------------------------------------------
-module(adapter_db).
-author("aaron lelevier").
-export([new/1, store/3, lookup/2, test/0]).
-include_lib("../macros.hrl").

-type db() :: #{} | ets:tab().

%% public
-spec new(StorageType) -> Db when
  StorageType :: map | ets,
  Db :: db().
new(map) -> maps:new();
new(ets) -> ets:new(?MODULE, []).

-spec store(Db, Key, Val) -> Db when
  Db :: #{} | ets:tab(),
  Key :: term(),
  Val :: term().
store(Db, Key, Val) when is_map(Db) ->
  maps:update(Key, Val, Db#{Key => Val});
store(Db, Key, Val) ->
  ets:insert(Db, {Key, Val}),
  Db.

-spec lookup(Db, Key) -> {ok, Val} | error when
  Db :: #{} | ets:tab(),
  Key :: term(),
  Val :: term().
lookup(Db, Key) when is_map(Db) -> maps:find(Key, Db);
lookup(Db, Key) ->
  case ets:lookup(Db, Key) of
    [] ->
      error;
    [{Key, Val}] ->
      {ok, Val}
  end.


%% tests
test() ->
  Key1 = foo,
  Val1 = bar,
  RandKey = bizbaz,

  % map
  Db = new(map),
  Db2 = store(Db, Key1, Val1),
  {ok, Val1} = lookup(Db2, Key1),
  error = lookup(Db2, RandKey),

  % ets
  Edb = new(ets),
  Edb = store(Edb, Key1, Val1),
  {ok, Val1} = lookup(Edb, Key1),
  error = lookup(Edb, RandKey),

  ?DEBUG("tests passed"),

  ok.