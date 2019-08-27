%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2019 05:56
%%%-------------------------------------------------------------------
-module(id3_v1).
-author("aaron lelevier").
-compile(export_all).
-import(lists, [filter/2, map/2, reverse/1]).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

test() -> dir(mp3_home()).

home() ->
  {ok, [[Home] | _]} = init:get_argument(home),
  Home.

mp3_home() ->
  lists:concat([home(), "/Documents/mp3"]).

dir(Dir) ->
  Files = lib_find:files(Dir, "*.mp3", true),

  % L1 = [{File, Parse}] where Parse = error | [{Tag, Val}]
  L1 = map(fun(I) -> {I, (catch read_id3_tag(I))} end, Files),

  % example of declaring a `fun` w/ multiple pattern matching signatures
  L2 = filter(
    fun({_, error}) -> false;
      (_) -> true
    end, L1),
  lib_misc:dump("mp3data", L2).

read_id3_tag(File) ->
  case file:open(File, [read, binary, raw]) of
    {ok, S} ->
      Size = filelib:file_size(File),
      {ok, B2} = file:pread(S, Size-128, 128),
      Result = parse_v1_tag(B2),
      file:close(S),
      Result;
    _Error ->
      error
  end.

parse_v1_tag(
    <<$T, $A, $G,
      Title:30/binary, Artist:30/binary,
      Album:30/binary, _Year:4/binary,
      _Comment:28/binary, 0:8, Track:8, _Genre:8>>) ->
  {"ID3v1.1",
    [{track, Track}, {title, trim(Title)},
      {artist, trim(Artist), {album, trim(Album)}}]}.

trim(Bin) ->
  list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(X) -> reverse(skip_blanks_and_zero(reverse(X))).

skip_blanks_and_zero([$\s|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero(X) -> X.

%% aaron

%% example of pattern matching on multiple `fun` signatures in line 1
%% returns: [true,false,false,true]
multiple_fun_clauses() ->
  IsTuple = fun({_,_}) -> true; (_) -> false end,
  lists:map(IsTuple, [{a,b}, 1, 3, {go,you}]).