%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2019 9:18 AM
%%%-------------------------------------------------------------------
-module(web).
-author("aaron lelevier").
-compile(export_all).
-export([]).

%% macros

-define(DEBUG(X), io:format("MOD:~p LINE:~p ~p~n", [?MODULE, ?LINE, X])).

init() -> ok.

%% jerseys list page

%% get-or-fetch the Jerseys list page
-spec jersey_list_page_content() -> binary().

jersey_list_page_content() ->
  Href = "jerseys-c102x3537065",
  Url = deeplink(Href),
  Filename = href_to_filename(Href),
  get_or_fetch(Url, Filename).

%% get-or-fetch helper - gets a local file if present or fetches it
get_or_fetch(Url, Filename) ->
  case file:read_file(Filename) of
    {error, enoent} ->
      {ok, Response} = httpc:request(Url),
      Bin = ahttp:response_to_binary(Response),
      ok = file:write_file(Filename, Bin),
      Bin;
    {ok, Bin} ->
      Bin
  end.

href_to_filename(Href) ->
  string:concat(Href, ".html").

string_jersey_list_page_content() ->
  Bin = jersey_list_page_content(),
  binary_to_list(Bin).

%% finds th first item's title
%% returns:
%%   "FOX FLEXAIR DELTA LONG SLEEVE JERSEY OPEN ORANGE 2019"
extract_title() ->
  StrH = "<h3 class=\"PBMainTxt\">",
  StrT = "</h3>",
  S = string_jersey_list_page_content(),
  [_ | T] = string:split(S, StrH),
  [H2 | _T2] = string:split(T, StrT),
  H2.

extract_titles() ->
  S = string_jersey_list_page_content(),
  extract_titles(S, []).

extract_titles([], Acc) -> Acc;
extract_titles(S, Acc) ->
  StrH = "<h3 class=\"PBMainTxt\">",
  StrT = "</h3>",
  [_ | T] = string:split(S, StrH),
  [H2 | T2] = string:split(T, StrT),
  ?DEBUG(H2),
  case H2 of
    [] ->
      lists:reverse(Acc);
    _ ->
      extract_titles(T2, [H2 | Acc])
  end.

acc_til_h3([Ha, Hb, Hc, Hd, He | T], Acc) ->
  case {Ha, Hb, Hc, Hd, He} of
    {$<, $/, $h, $3, $>} ->
      Acc;
    _ ->
      acc_til_h3(T, [Hb, Hc, Hd, He | Acc])
  end;
acc_til_h3([], Acc) ->
  lists:reverse(Acc).

%% extracts text by a start and end
extract(Str, Start, End) ->
  [_ | T] = string:split(Str, Start),
  [H2 | _] = string:split(T, End),
  H2.

get_item() ->
  S = string_jersey_list_page_content(),
  X = "<tr class=\"viewItemList\">",
  [_ | T] = string:split(S, X),
  % this is the DOM for the 1st item, but each item is a table, w/ a nested table inside
  % that doesn't have much, so capturing the 1st closed table gets all DOM we're interested in
  [H2 | _] = string:split(T, "</table>"),
  H2.

%% returns the relative href to an item
%%-> "fox-flexair-delta-long-sleeve-jersey-open-orange-2019-c2x28592930"
get_item_href() ->
  Item = get_item(),
  [_ | T4] = string:split(Item, "href=\""),
  [H5 | _] = string:split(T4, "\""),
  H5.

get_item_url() ->
  Href = get_item_href(),
  deeplink(Href).

%% returns the deeplink to an item
deeplink(Href) ->
  BaseUrl = "https://www.commencalusa.com/",
  string:concat(BaseUrl, Href).

get_item_data() ->
  H2 = get_item(),
  Price = get_price(H2),
  Title = get_title(H2),
  {{price, Price}, {title, Title}}.

%%-> "$ 99.90"
get_price(H2) ->
  Start = "<span class=\"PBSalesPrice\">",
  End = "</span>",
  extract(H2, Start, End).

%%-> "FOX FLEXAIR DELTA LONG SLEEVE JERSEY OPEN ORANGE 2019"
get_title(H2) ->
  Start2 = "<h3 class=\"PBMainTxt\">",
  End2 = "</h3>",
  extract(H2, Start2, End2).

%% Jersey data
%% title: FOX FLEXAIR DELTA LONG SLEEVE JERSEY OPEN ORANGE 2019

string_to_binstring(S) ->
  %%  S = "<h3 class=\"PBMainTxt\">"
  L = [string:concat([X], "$ ") || X <- S],
  S2 = string:trim(concat(L, [])),
  list_to_tuple(string:lexemes(lists:reverse(S2), [$\s])).

concat([], Acc) ->
  Acc;
concat([H | T], Acc) ->
  Acc2 = string:concat(H, Acc),
  concat(T, Acc2).

%% jerseys detail page

jersey_detail_page_content() ->
  Href = get_item_href(),
  Url = deeplink(Href),
  Filename = href_to_filename(Href),
  get_or_fetch(Url, Filename).

string_jersey_detail_page_content() ->
  Bin = jersey_detail_page_content(),
  binary_to_list(Bin).

%% item data

get_item_name(S) ->
  extract(S, "<h1 class=\"PBMainTxt PBItemTitle\">", "</h1>").

log_get_item_price(S) ->
  {SalesPrice, StrikePrice} = get_prices(S),
  ?DEBUG({StrikePrice, SalesPrice}).

get_prices(S) ->
  SalesPrice = extract(S, "<span class=\"PBSalesPrice\">", "</span>"),
  StrikePrice = get_value(extract(S, "<div class=\"PBStrike\">", "</div>")),
  {SalesPrice, StrikePrice}.

get_item_price(S) ->
  {SalesPrice, StrikePrice} = get_prices(S),
  case StrikePrice of
    undefined ->
      SalesPrice;
    _ ->
      StrikePrice
  end.

get_item_sale_price(S) ->
  {SalesPrice, StrikePrice} = get_prices(S),
    case StrikePrice of
      undefined ->
        undefined;
    _ ->
      SalesPrice
  end.

%% returns str %
get_item_discount(S) ->
  get_value(extract(S, "<span class=\"PBDiscount\">", "</span>")).

get_value(Value) ->
  case Value of
    [] ->
      undefined;
    Value2 ->
      Value2
  end.

%% the first word of the "name" is (sometimes) the brand name
get_item_brand(S) ->
  Title = get_item_name(S),
  [Brand|_] = string:split(Title, " "),
  Brand.

%% returns the image Url of the item
%%-> "http://www.commencalusa.com/Files/106799/Img/12/T19MTMLFOXFLDEOO_675.jpg"
get_item_image_url(S) ->
  extract(S, "<meta property=\"og:image\" content=\"", "\" />").

%% returns a list of available sizes
%%-> ["S","M","L","XL"]
get_sizes(S) ->
  Sizes = web:extract(S, "class=\"facettypevaluegrid facetlist\">", "<input"),
  get_sizes(Sizes, []).

get_sizes(S, Acc) ->
  Start = "<span class=\"Facetvaluename\">",
  End = "</span>",
  [_|T] = string:split(S, Start),
  [H1|T1] = string:split(T, End),
  case H1 of
    [] ->
      lists:reverse(Acc);
    _ ->
      get_sizes(T1, [H1|Acc])
  end.

%% item data - testing

get_item_name() ->
  S = string_jersey_detail_page_content(),
  get_item_name(S).

get_item_price() ->
  S = string_jersey_detail_page_content(),
  get_item_price(S).

%% test sale item
string_jersey_detail_page_content_on_sale() ->
  Href = "jersey-long-commencal-by-dakine-c2x23830963",
  Url = deeplink(Href),
  Filename = href_to_filename(Href),
  Bin = get_or_fetch(Url, Filename),
  binary_to_list(Bin).

get_item_sale_price() ->
  %% NOTE: the Href is hardcoded b/c the 1st item being tested
  %% above doesn't have a sale price
  S = string_jersey_detail_page_content_on_sale(),
  get_item_sale_price(S).

get_item_discount() ->
  S = string_jersey_detail_page_content_on_sale(),
  get_item_discount(S).

get_item_brand() ->
  S = get_item_name(),
  get_item_brand(S).

get_item_image_url() ->
  S = string_jersey_detail_page_content(),
  get_item_image_url(S).

get_sizes() ->
  S = string_jersey_detail_page_content(),
  get_sizes(S).
