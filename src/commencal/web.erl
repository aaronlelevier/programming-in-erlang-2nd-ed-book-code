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
-include("macros.hrl").

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

%% returns a list of hrefs for the detail page of each jersey
extract_all_jersey_detail_hrefs() ->
  S = string_jersey_list_page_content(),
  Start = "<a  href=\"",
  End = "\" title=",
  extract_all(S, Start, End).

extract_all(Str, Start, End) ->
  extract_all(Str, Start, End, []).

extract_all(Str, Start, End, Acc) ->
  [_ | T] = string:split(Str, Start),
  [H2 |T2] = string:split(T, End),
  case H2 of
    [] ->
      lists:reverse(Acc);
    _ ->
    extract_all(T2, Start, End, [H2|Acc])
  end.

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
