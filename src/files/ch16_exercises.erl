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

check_compiled_files() ->
  Files = lib_find:files(".", "*.erl", true),
  check_compiled_files2(Files).

check_compiled_files2([]) -> ok;
check_compiled_files2([H | T]) ->
  {ok, Info1} = file:read_file_info(H),
  {ok, Info2} = file:read_file_info(beam_file(H)),
  case dtime:same_ish(Info1#file_info.mtime, Info2#file_info.mtime) of
    true ->
      io:fwrite("Y - ~p~n", [H]);
    false ->
      io:fwrite("N - ~p~n", [H])
  end,
  check_compiled_files2(T).

-spec beam_file(Filename) -> {ok, BeamFilename} | error when
  Filename :: string(),
  BeamFilename :: string().

beam_file(Filename) ->
  [S1, S2 | _] = string:replace(
    Filename, filename:extension(Filename), ".beam"),
  string:concat(S1, S2).


%% ex-2 - compute the md5 hash of a file

%% TODO: could add a switch that checks file size then either calls small or large function

%% TODO: could rename to `small_file_md5`

file_md5(Filename) ->
  case file:read_file(Filename) of
    {ok, Contents} ->
      % `crypto:hash(md5, Contents)` could also be used here
      erlang:md5(Contents);
    {error, Reason} ->
      {error, Reason}
  end.


%% ex-3 - compute the hash of a file that is ~100 MBs using:
%% - erlang:md5_init
%% - erlang:md5_update
%% - erlang:md5_final

large_file_md5(Filename) ->
  Context = erlang:md5_init(),
  case file:open(Filename, [read, binary, raw]) of
    {ok, IoDevice} ->
      large_file_md5(IoDevice, Context, 0);
    {error, Reason} ->
      {error, Reason}
  end.

large_file_md5(IoDevice, Context, Index) ->
  % if increment of hashing is too small, not performant
  Incr = 10000,
  case file:pread(IoDevice, Index, Incr) of
    {ok, Data} ->
      NewContext = erlang:md5_update(Context, Data),
      large_file_md5(IoDevice, NewContext, Index + Incr);
    eof ->
      erlang:md5_final(Context);
    {error, Reason} ->
      {error, Reason}
  end.


%% ex-4 - compute the md5 of JPGs to see if any are the same

unique_jpgs(Dir) ->
  Jpgs = lib_find:files(Dir, "*.jpg", false),
  Hashes = [file_md5(F) || F <- Jpgs],
  check_unique_jpgs(lists:zip(Jpgs, Hashes)).

check_unique_jpgs(L) ->
  check_unique_jpgs1(L, sets:new()).

check_unique_jpgs1([H|T], Set) ->
  {File, Bin} = H,
  InitSize = sets:size(Set),
  Set2 = sets:add_element(Bin, Set),
  NewSize = sets:size(Set2),
  if InitSize =:= NewSize ->
      io:fwrite("Existing JPG:~p~n", [File]);
    true ->
      io:fwrite("New JPG:~p~n", [File])
  end,
  check_unique_jpgs1(T, Set2);
check_unique_jpgs1([], _Set) ->
  ok.


%% ex-5 - use cache to store md5 checksum and recalculate when file is modified

start_cached_md5() ->
  register(md5_cache, spawn(?MODULE, cached_md5_server, [])).

%% client
get_cached_md5(File) ->
  LastModified = last_modified(File),
  md5_cache ! {self(), {File, LastModified}},
  receive
    {md5_cache, {IsFromCache, Md5}} ->
      {IsFromCache, Md5};
    Error ->
      {error, Error}
  after 500 ->
    client_timeout
  end.

cached_md5_server() ->
  % cache - key is Filename, value is a tuple of {LastModified, Md5Bin}
  Cache = #{},
  cached_md5_server_loop(Cache).

%% TODO: `cached_md5_server_loop` is probably doing too much

cached_md5_server_loop(Cache) ->
  receive
    {From, {File, NewLastModified}} ->
      try maps:get(File, Cache) of
        {LastModified, Md5Bin} ->
          io:fwrite("Existing JPG:~p~n", [File]),
          if LastModified =:= NewLastModified ->
              io:fwrite("retrieving from cache~n"),
              From ! {md5_cache, {true, Md5Bin}},
              cached_md5_server_loop(Cache);
            true ->
              io:fwrite("bust cache~n"),
              NewMd5Bin = file_md5(File),
              NewCache = Cache#{File => {NewLastModified, NewMd5Bin}},
              From ! {md5_cache, {false, NewMd5Bin}},
              cached_md5_server_loop(NewCache)
          end
      catch
        error:{badkey, Key} ->
          io:fwrite("New JPG:~p~n", [Key]),
          io:fwrite("adding to cache~n"),
          NewMd52 = file_md5(File),
          NewCache2 = Cache#{File => {last_modified(File), NewMd52}},
          From ! {md5_cache, {false, NewMd52}},
          cached_md5_server_loop(NewCache2)
      end;
    Error ->
      {error, Error}
  end.

last_modified(File) ->
  {ok, Info} = file:read_file_info(File),
  Info#file_info.mtime.

%% tests

test() ->
  ok = test_large_file_md5(),
  ok.

test_large_file_md5() ->
  File = "/Users/aaron/Downloads/STRICTLY-REGGAE.mp3",

  Context = large_file_md5(File),

  true = is_binary(Context),
  true = size(Context) > 0,
  ok.