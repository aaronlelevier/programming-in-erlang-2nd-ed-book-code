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
  Bool = filelib:is_file(File),
  case dets:open_file(?MODULE, [{file, File}]) of
    {ok, ?MODULE} ->
      case Bool of
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

%% use to look up a key directly, for debugging
lookup(Key) ->
  dets:lookup(?MODULE, Key).

test() ->
  File = random_filename(),
  true = open(File),

  Ret = try try_tests(File) of
    ok ->
      ok
  catch
    _:Why ->
      {error, Why}
  end,

  % cleanup test file for `dets` by closing and deleting
  close(),
  file:delete(File),

  Ret.

try_tests(File) ->
  io:format("tests wrapped in try/catch for File:~p~n", [File]),
  [{free, 1}] = lookup(free),
  ok.

random_filename() ->
  "test-" ++ integer_to_list(random_integer(10)).

random_integer(Size) ->
  round(rand:uniform() * math:pow(10, Size)).