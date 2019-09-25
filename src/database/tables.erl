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
  ok = try_insert_init_data(),
  ok.

try_insert_init_data() ->
  try_insert_init_data(1).

try_insert_init_data(Counter) ->
  case reset_tables() of
    {atomic, ok} ->
      io:format("Tables populated~n"),
      io:format("~p~n", [select_all()]),
      ok;
    {aborted, Error} ->
      io:format("Error:~p~n", [Error]),
      if Counter < 5 ->
        timer:sleep(500),
        try_insert_init_data(Counter + 1);
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

%% helpers

do_this_once() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
  mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
  mnesia:stop().

do(Q) ->
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

