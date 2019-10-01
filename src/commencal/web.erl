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

%% returns the initial page content that we're interested in
%% will first try to get the page contents from a local cache
-spec jersey_list_page_content() -> binary().

jersey_list_page_content() ->
  Url = "https://www.commencalusa.com/jerseys-c102x3537065",
  Filename = "jerseys.txt",
  case file:read_file(Filename) of
    {error, enoent} ->
      {ok, Response} = httpc:request(Url),
      Bin = ahttp:response_to_binary(Response),
      ok = file:write_file(Filename, Bin),
      Bin;
    {ok, Bin} ->
      Bin
  end.

string_jersey_list_page_content() ->
  Bin = jersey_list_page_content(),
  binary_to_list(Bin).

%% finds th first item's title
%% returns:
%%   "FOX FLEXAIR DELTA LONG SLEEVE JERSEY OPEN ORANGE 2019"
extract_title() ->
  StrH = "<h3 class=\"PBMainTxt\">",
  StrT =  "</h3>",
  S = string_jersey_list_page_content(),
  [_|T] = string:split(S, StrH),
  [H2|_T2] = string:split(T, StrT),
  H2.

extract_titles() ->
  S = string_jersey_list_page_content(),
  extract_titles(S, []).

extract_titles([], Acc) -> Acc;
extract_titles(S, Acc) ->
  StrH = "<h3 class=\"PBMainTxt\">",
  StrT =  "</h3>",
  [_|T] = string:split(S, StrH),
  [H2|T2] = string:split(T, StrT),
  ?DEBUG(H2),
  case H2 of
    [] ->
      lists:reverse(Acc);
    _ ->
      extract_titles(T2, [H2|Acc])
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
  [_|T] = string:split(Str, Start),
  [H2|_] = string:split(T, End),
  H2.

get_item() ->
  S = string_jersey_list_page_content(),
  X = "<tr class=\"viewItemList\">",
  [_|T] = string:split(S, X),
  % this is the DOM for the 1st item, but each item is a table, w/ a nested table inside
  % that doesn't have much, so capturing the 1st closed table gets all DOM we're interested in
  [H2|_] = string:split(T, "</table>"),
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
concat([H|T], Acc) ->
  Acc2 = string:concat(H, Acc),
  concat(T, Acc2).