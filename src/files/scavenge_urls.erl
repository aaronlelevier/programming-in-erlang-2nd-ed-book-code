%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2019 06:42
%%%-------------------------------------------------------------------
-module(scavenge_urls).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

urls2htmlFile(Urls, File) ->
  file:write_file(File, urls2html(Urls)).

bin2urls(Bin) -> gather_urls(binary_to_list(Bin), []).

urls2html(Urls) ->
  [h1("Urls"), make_list(Urls)].

h1(Title) -> ["<h1>", Title, "</h1>"].

make_list(L) ->
  ["<ul>\n", lists:map(fun(I) -> ["<li>", I, "</li>\n"] end, L), "</ul>\n"].

gather_urls([], L) -> L.