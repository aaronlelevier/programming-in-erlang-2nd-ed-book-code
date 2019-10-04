%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2019 6:06 AM
%%%-------------------------------------------------------------------
-module(db).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

% placeholder `init` for Makefile
init() -> ok.

% main loop
start_and_load_data() ->
  % starts DB
  mnesia:start(),
  % fetches Items and inserts in DB
  comb:fetch_and_insert().

create_tables() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(item, [{attributes, record_info(fields, item)},
    {disc_copies, [node()]}]),
  mnesia:stop().

insert(Row) ->
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F).

-spec select(atom()) -> [item].
%% select all for a given `table`
select(Table) ->
  do(qlc:q([X || X <- mnesia:table(Table)])).

-spec select_by_size(string()) -> [item].
%% returns a filtered list of `item` records by size
select_by_size(Size) ->
  BinSize = list_to_binary(Size),
  do(qlc:q([X || X <- mnesia:table(item),
    lists:member(BinSize, X#item.sizes) =:= true]
  )).

%% helpers

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.
