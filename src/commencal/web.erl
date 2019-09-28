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

%% TODO: next