%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2019 05:30
%%%-------------------------------------------------------------------
-module(twit_store).
-author("aaron lelevier").
-chapter(["16"]).
-include_lib("kernel/include/file.hrl").

-compile(export_all).
-export([]).

%% the filename to be used for storage
-define(FILENAME, "data/twit-store.txt").

%% the size of a twit
-define(TWIT_SIZE, 140).

% placeholder `init` for Makefile
init0() -> ok.

start() ->
  K = 10,
  init(K),
  % store twit at start of buf
  store(0, "I went mtb'ing yesterday"),
  fetch(0),
  store(3, "I am Bob"),
  fetch(3),
  % store twit at end of buf
  store(10, "You are Gary"),
  fetch(10),
  % fetch a non existent twit
  fetch(5).


%% functions from ex-6

init(K) ->
  io:format("init - K:~p~n", [K]),
  IoDevice = open_for_write(),
  file:pwrite(IoDevice, file_length(K), <<"\n">>),
  close(IoDevice).

store(N, Str) ->
  io:format("store - N:~p Str:~p~n", [N, Str]),
  Bin = pad(Str),
  store_buf(N, Bin).


fetch(N) ->
  io:format("fetch - N:~p~n", [N]),
  IoDevice = open_for_read(),
  case file:pread(IoDevice, file_location(N), file_location(N + 1)) of
    {ok, Bin} ->
      close(IoDevice),
      Length = binary_tl_end(Bin),
      <<Bin2:Length/binary, _/binary>> = Bin,
      TwitStr = binary_to_list(Bin2),
      case lists:sum(TwitStr) of
        % throws an error if you try to access a non existent twit
        0 ->
          error(bad_index, [N]);
        % success case
        _ ->
          io:format("fetch - TwitStr:~p~n", [TwitStr]),
          TwitStr
      end;
    Error ->
      close(IoDevice),
      Error
  end.


%% helpers

file_location(N) ->
  N * ?TWIT_SIZE.

store_buf(N, Bin) ->
  IoDevice = open_for_write(),
  file:pwrite(IoDevice, file_location(N), Bin),
  close(IoDevice).

open_for_write() ->
  {ok, IoDevice} = file:open(?FILENAME, [write, raw, binary]),
  IoDevice.

open_for_read() ->
  {ok, IoDevice} = file:open(?FILENAME, [read, raw, binary]),
  IoDevice.

close(IoDevice) ->
  file:close(IoDevice).

size() ->
  case file:read_file_info(?FILENAME) of
    {ok, Facts} ->
      Facts#file_info.size;
    {error, Reason} ->
      {error, Reason}
  end.

%% needs to be the desired file length - 1 because the \n newline
%% character adds 1 char to end of file
file_length(K) ->
  K * ?TWIT_SIZE - 1.

pad(Str) ->
  Bin = list_to_binary(Str),
  PadSize = (?TWIT_SIZE - size(Bin)) * 8,
  Length = length(Str),
  <<Bin/binary, Length:PadSize>>.

tl_end(L) ->
  hd(lists:reverse(L)).

binary_tl_end(Bin) ->
  tl_end(binary_to_list(Bin)).


%% example code - file:pread

%%1> {ok, S} = file:open("data1.dat", [read,binary,raw]).
%%{ok,{file_descriptor,prim_file,{#Port<0.106>,5}}}
%%2> file:pread(S, 22, 46).
%%{ok,<<"rong\",\n\t[{occupation, programmer},\n\t {favorite">>}
%%3> file:pread(S, 1, 10).
%%{ok,<<"person, \"j">>}
%%4> file:pread(S, 2, 10).
%%{ok,<<"erson, \"jo">>}
%%5> file:close(S).
%%ok