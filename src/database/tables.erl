%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Sep 2019 6:11 AM
%%%-------------------------------------------------------------------
-module(tables).
-author("aaron lelevier").
-compile(export_all).
-export([]).

%% records
-include_lib("stdlib/include/qlc.hrl").

-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).
% example record for storing complex data types in `mnesia`
-record(design, {id, plan}).

%% put initial table data here so it's easier to see alongside the
%% record field names

example_data() ->
  [% shop: item, quantity, cost
    {shop, apple, 20, 2.3},
    {shop, orange, 100, 3.8},
    {shop, pear, 200, 3.6},
    {shop, banana, 420, 4.5},
    {shop, potato, 2456, 1.2},
    % cost: name, price
    {cost, apple, 1.5},
    {cost, orange, 2.4},
    {cost, pear, 2.2},
    {cost, banana, 1.5},
    {cost, potato, 0.6}
  ].

%% insert initial data

init() ->
  ok = mnesia:start(),
  ok = try_insert_init_data(fun reset_tables/0),
  ok.

try_insert_init_data(Fun) ->
  try_insert_init_data(Fun, 1).

try_insert_init_data(Fun, Counter) ->
  case Fun() of
    {atomic, ok} ->
      io:format("Tables populated~n"),
      io:format("~p~n", [select_all()]),
      ok;
    {aborted, Error} ->
      io:format("Error:~p~n", [Error]),
      if Counter < 5 ->
        timer:sleep(500),
        try_insert_init_data(Fun, Counter + 1);
        true ->
          Error
      end
  end.

%% queries

select_all() ->
  do(qlc:q([X || X <- mnesia:table(shop)])).

select_columns() ->
  do(qlc:q([{X#shop.item, X#shop.quantity} || X <- mnesia:table(shop)])).

select_where() ->
  do(qlc:q([{X#shop.item, X#shop.quantity} ||
    X <- mnesia:table(shop),
    X#shop.quantity < 250
  ])).

select_join() ->
  do(qlc:q([{X#shop.item, Y#cost.price, X#shop.cost} ||
    X <- mnesia:table(shop),
    Y <- mnesia:table(cost),
    X#shop.item =:= Y#cost.name
  ])).

%% add / update data

upsert() ->
  io:format("start with empty tables~n"),
  mnesia:clear_table(shop),
  log_shop_table_data(),

  % add melon
  write_row(#shop{item = melon, quantity = 10, cost = 2.0}),
  log_shop_table_data(),

  % add apples
  write_row(#shop{item = apple, quantity = 20, cost = 0.5}),
  log_shop_table_data(),

  % update melon
  % NOTE: must insert all fields, or else field value will be 'undefined`
  write_row(#shop{item = melon, quantity = 1, cost = 2.1}),
  log_shop_table_data().

write_row(Row) ->
  F = fun() -> mnesia:write(Row) end,
  mnesia:transaction(F).

log_table_data(Item) ->
  Data = do(qlc:q([X || X <- mnesia:table(Item)])),
  io:format("~p~n", [Data]).

log_shop_table_data() ->
  log_table_data(shop).

%% deleting data
%% NOTE: if `Oid` doesn't exist, no error is raised

delete({_Table, _Id} = Oid) ->
  log_shop_table_data(),

  F = fun() -> mnesia:delete(Oid) end,
  mnesia:transaction(F),

  log_shop_table_data().

% transaction examples
% uses `mnesia:abort` w/i the `fun` to abort a transaction based
% upon some condition
aborting_a_transaction(Nwant) ->
  reset_tables(),

  % {orange, item, quantity, cost}
  F = fun() -> mnesia:read({shop, orange}) end,
  {atomic, [Orange]} = mnesia:transaction(F),
  {shop, orange, Quantity, _Cost} = Orange,
  100 = Quantity,

  %% as a transaction, show that an abort works
  F2 = fun() ->
    if Nwant =< Quantity ->
      Orange1 = Orange#shop{quantity = Quantity - Nwant},
      mnesia:write(Orange1);
      true ->
        Reason = oranges,
        % http://erlang.org/doc/man/mnesia.html#abort-1
        mnesia:abort(Reason)
    end
       end,
  mnesia:transaction(F2).

%% helpers

do_this_once() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
  mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
  mnesia:create_table(design, [{attributes, record_info(fields, design)}]),
  mnesia:stop().

do(Q) ->
  % NOTE: `qlc:e` - evaluates a query handle and returns all
  % answers in a list
  % http://erlang.org/doc/man/qlc.html#e-1
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

reset_tables() ->
  mnesia:clear_table(shop),
  mnesia:clear_table(cost),
  F = fun() ->
    lists:foreach(fun mnesia:write/1, example_data())
      end,
  mnesia:transaction(F).

insert_design_data() ->
  io:format("initial 'design' table data~n"),
  log_table_data(design),

  D1 = #design{id = {joe, 1},
    plan = {circle, 10}},
  D2 = #design{id = fred,
    plan = {rectangle, 10, 5}},
  D3 = #design{id = {jane, {house, 23}},
    plan = {house,
      [{floor, 1,
        [{doors, 3},
          {windows, 12},
          {rooms, 5}]},
        {floor, 2,
          [{doors, 2},
            {rooms, 4},
            {windows, 15}]}]}},
  F = fun() ->
    mnesia:write(D1),
    mnesia:write(D2),
    mnesia:write(D3)
      end,
  mnesia:transaction(F),

  io:format("udpated 'design' table data~n"),
  log_table_data(design).

read_design(OId) ->
  F = fun() -> mnesia:read({design, OId}) end,
  mnesia:transaction(F).
