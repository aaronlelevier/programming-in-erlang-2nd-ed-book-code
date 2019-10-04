%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Combines `web` and `item` to fetch all detail jersey pages
%%% @end
%%% Created : 03. Oct 2019 6:06 AM
%%%-------------------------------------------------------------------
-module(comb).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-include("macros.hrl").

% placeholder `init` for Makefile
init() -> ok.

%% main loop
fetch_and_insert() ->
  Pid = spawn(fun() -> master() end),
  [spawn(fun() -> worker(Pid, Href) end) ||
    Href <- web:extract_all_jersey_detail_hrefs()],
  Pid.

%% master - has a catch-all receive and logs messages
master() ->
  receive
    Msg ->
      ?DEBUG(Msg),
      {_Pid, _Href, Item} = Msg,
      db:insert(Item),
      master()
  after 2000 ->
    ?DEBUG("master timeout~n")
  end.

%% the worker is in charge of fetching the page, converting it to an `item`
%% record, and telling the `master` about it
worker(MasterPid, Href) ->
  % http request
  S = web:jersey_detail_page_content(Href),
  % record
  Record = item:init(S),
  MasterPid ! {self(), Href, Record}.