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
  [H2|_] = string:split(T, StrT),
  H2.

acc_til_h3([Ha, Hb, Hc, Hd, He | T], Acc) ->
  case {Ha, Hb, Hc, Hd, He} of
    {$<, $/, $h, $3, $>} ->
      Acc;
    _ ->
      acc_til_h3(T, [Hb, Hc, Hd, He | Acc])
  end;
acc_til_h3([], Acc) ->
  lists:reverse(Acc).

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