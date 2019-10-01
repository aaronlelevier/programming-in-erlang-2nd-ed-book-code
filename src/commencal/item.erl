%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Oct 2019 6:41 AM
%%%-------------------------------------------------------------------
-module(item).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `start` for Makefile
start() -> ok.

-record(item, {name, price, sale_price, discount, brand, image_url,
  sizes}).

init() ->
  S = web:string_jersey_detail_page_content(),
  init(S).

init(S) ->
  #item{
    name = web:get_item_name(S),
    price = web:get_item_price(S),
    sale_price = web:get_item_sale_price(S),
    discount = web:get_item_discount(S),
    brand = web:get_item_brand(S),
    image_url = web:get_item_image_url(S),
    sizes = web:get_sizes(S)
  }.

