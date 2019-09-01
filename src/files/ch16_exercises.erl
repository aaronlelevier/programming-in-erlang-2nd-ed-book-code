%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2019 06:10
%%%-------------------------------------------------------------------
-module(ch16_exercises).
-author("aaron lelevier").
-chapter(["16"]).
-compile(export_all).
-export([]).
-include_lib("kernel/include/file.hrl").

% placeholder `init` for Makefile
init() -> ok.

%% ex-1 - checks the current dir for *.erl files and outputs a bool
%% whether or not the *.beam file has been compiled within a minute
%% of the *.erl file

one() ->
  Files = lib_find:files(".", "*.erl", true),
  one_check(Files).

one_check([]) -> ok;
one_check([H|T]) ->
  {ok, Info1} = file:read_file_info(H),
  {ok, Info2} = file:read_file_info(beam_file(H)),
  case dtime:same_ish(Info1#file_info.mtime, Info2#file_info.mtime) of
    true ->
      io:fwrite("Y - ~p~n", [H]);
    false ->
      io:fwrite("N - ~p~n", [H])
  end,
  one_check(T).

-spec beam_file(Filename) -> {ok, BeamFilename} | error when
  Filename :: string(),
  BeamFilename :: string().

beam_file(Filename) ->
  [S1,S2|_] = string:replace(
    Filename, filename:extension(Filename), ".beam"),
  string:concat(S1,S2).