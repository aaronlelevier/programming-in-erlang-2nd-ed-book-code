%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2019 5:53 AM
%%%-------------------------------------------------------------------
-module(item_test).
-author("aaron lelevier").
-compile(export_all).
-export([]).

-import(item, [name/1, price/1, sale_price/1, discount/1, brand/1, image_url/1, sizes/1]).
-import(web, [string_jersey_detail_page_content/0, deeplink/1, href_to_filename/1, get_or_fetch/2,
string_jersey_detail_page_content_on_sale/0]).

% placeholder `init` for Makefile
init() -> ok.

name_test() ->
  S = string_jersey_detail_page_content(),
  name(S).

price_test() ->
  S = string_jersey_detail_page_content(),
  price(S).

%% test sale item
string_jersey_detail_page_content_on_sale_test() ->
  Href = "jersey-long-commencal-by-dakine-c2x23830963",
  Url = deeplink(Href),
  Filename = href_to_filename(Href),
  Bin = get_or_fetch(Url, Filename),
  binary_to_list(Bin).

sale_price_test() ->
  %% NOTE: the Href is hardcoded b/c the 1st item being tested
  %% above doesn't have a sale price
  S = string_jersey_detail_page_content_on_sale(),
  sale_price(S).

discount_test() ->
  S = string_jersey_detail_page_content_on_sale(),
  discount(S).

brand_test() ->
  S = string_jersey_detail_page_content_on_sale(),
  brand(S).

image_url_test() ->
  S = string_jersey_detail_page_content(),
  image_url(S).

get_sizes_test() ->
  S = string_jersey_detail_page_content(),
  sizes(S).
