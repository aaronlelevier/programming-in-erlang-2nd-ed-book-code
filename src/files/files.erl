%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2019 06:44
%%%-------------------------------------------------------------------
-module(files).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

func() ->
{person, "joe", "armstrong",
  [{occupation, programmer},
    {favoriteLangage, erlang}]}.

func2() ->
{cat, {name, "zorro",
  {owner, "joe"}}}.


%% unconsult - convert a list of Terms to a file

unconsult(Filename, Terms) ->
  case file:open(Filename, write) of
    {ok, S} ->
      lists:map(fun(X) -> io:format(S, "~p.~n", [X]) end, Terms),
      file:close(S);
    {error, Why} ->
      {error, Why}
  end.


%% consult example implementation

consult(File) ->
  case file:open(File, read) of
    {ok, S} ->
      Val = consult1(S),
      file:close(S),
      Val;
    {error, Why} ->
      {error, Why}
  end.

consult1(S) ->
  case io:read(S, '') of
    {ok, Term} -> [Term|consult1(S)];
    eof -> [];
    {error, Why} -> {error, Why}
  end.