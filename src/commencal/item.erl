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
-include("macros.hrl").
-include("records.hrl").

% placeholder `start` for Makefile
start() -> ok.

init() -> ok.

init(S) ->
  #item{
    name = name(S),
    price = price(S),
    sale_price = sale_price(S),
    discount = discount(S),
    brand = brand(S),
    image_url = image_url(S),
    sizes = sizes(S)
  }.

name(S) ->
  web:extract(S, "<h1 class=\"PBMainTxt PBItemTitle\">", "</h1>").

log_price(S) ->
  {SalesPrice, StrikePrice} = prices(S),
  ?DEBUG({StrikePrice, SalesPrice}).

prices(S) ->
  SalesPrice = web:extract(S, "<span class=\"PBSalesPrice\">", "</span>"),
  StrikePrice = get_value(web:extract(S, "<div class=\"PBStrike\">", "</div>")),
  {SalesPrice, StrikePrice}.

price(S) ->
  {SalesPrice, StrikePrice} = prices(S),
  case StrikePrice of
    undefined ->
      SalesPrice;
    _ ->
      StrikePrice
  end.

sale_price(S) ->
  {SalesPrice, StrikePrice} = prices(S),
  case StrikePrice of
    undefined ->
      undefined;
    _ ->
      SalesPrice
  end.

%% returns str %
discount(S) ->
  get_value(web:extract(S, "<span class=\"PBDiscount\">", "</span>")).

get_value(Value) ->
  case Value of
    [] ->
      undefined;
    Value2 ->
      Value2
  end.

%% the first word of the "name" is (sometimes) the brand name
brand(S) ->
  Title = name(S),
  [Brand|_] = string:split(Title, " "),
  Brand.

%% returns the image Url of the item
%%-> "http://www.commencalusa.com/Files/106799/Img/12/T19MTMLFOXFLDEOO_675.jpg"
image_url(S) ->
  web:extract(S, "<meta property=\"og:image\" content=\"", "\" />").

%% returns a list of available sizes
%%-> ["S","M","L","XL"]
sizes(S) ->
  Sizes = web:extract(S, "class=\"facettypevaluegrid facetlist\">", "<input"),
  sizes(Sizes, []).

sizes(S, Acc) ->
  Start = "<span class=\"Facetvaluename\">",
  End = "</span>",
  [_|T] = string:split(S, Start),
  [H1|T1] = string:split(T, End),
  case H1 of
    [] ->
      lists:reverse(Acc);
    _ ->
      sizes(T1, [H1|Acc])
  end.

