%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2019 6:42 AM
%%%-------------------------------------------------------------------
-module(dets_test).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

open(File) ->
  io:format("dets opened:~p~n", [File]),
  case dets:open_file(?MODULE, [{file, File}]) of
    {ok, ?MODULE} ->
      case filelib:is_file(File) of
        true -> void;
        false ->
          ok = dets:insert(?MODULE, {free, 1})
      end,
      true;
    {error, Reason} ->
      io:format("cannot open dets table~n"),
      exit({eDetsOpen, File, Reason})
  end.

close() -> dets:close(?MODULE).

filename2index(FileName) when is_binary(FileName) ->
  case dets:lookup(?MODULE, FileName) of
    [] ->
      [{_, Free}] = dets:lookup(?MODULE, free),
      ok = dets:insert(?MODULE,
        [{Free, FileName}, {FileName, Free}, {free, Free}]),
      Free;
    [{_, N}] ->
      N
  end.

index2filename(Index) when is_integer(Index) ->
  case dets:lookup(?MODULE, Index) of
    [] -> error;
    [{_, Bin}] -> Bin
  end.

%% aaron

lookup(Key) ->
  dets:lookup(?MODULE, Key).