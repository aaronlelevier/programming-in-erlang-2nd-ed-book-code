%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2019 6:51 AM
%%%-------------------------------------------------------------------
-module(api).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-import(ezwebframe_mochijson2, [encode/1]).

% placeholder `init` for Makefile
init() -> ok.

item_to_list() ->
  Keys = [item, name, price, sale_price, discount, brand, image_url, sizes],

  % TODO: can replace w/ `db:select_first/0` ... if accessing the DB
  % this would be the value returned from a single Mnesia record
  Values = tuple_to_list({item,<<"FOX FLEXAIR DELTA LONG SLEEVE JERSEY OPEN ORANGE 2019">>,
    <<"$ 99.90">>,undefined,undefined,<<"FOX">>,
    <<"http://www.commencalusa.com/Files/106799/Img/12/T19MTMLFOXFLDEOO_675.jpg">>,
    [<<"S">>,<<"M">>,<<"L">>,<<"XL">>]}),

  lists:zip(Keys, Values).

item_to_json() ->
    encode(item_to_list()).
