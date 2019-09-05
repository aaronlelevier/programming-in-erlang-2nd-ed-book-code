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
-define(TWIT_SIZE, 10).

% placeholder `init` for Makefile
init0() -> ok.

%% functions from ex-6

init(K) ->
  IoDevice = open_for_write(),
  file:pwrite(IoDevice, file_length(K), <<"\n">>),
  close(IoDevice).

store(N, Str) ->
  Bin = pad(Str),
  store_buf(N, Bin).


%% TODO: need to store string size in buffer, maybe at end, so the
%% TODO: buffer can be destructured to retrieve the string
%%130> twit_store:store(0, "Hello").
%%ok
%%131> twit_store:fetch(0).
%%[72,101,108,108,111,0,0,0,0,10]
%%132> Bin = <<72,101,108,108,111,0,0,0,0,10>>.
%%<<72,101,108,108,111,0,0,0,0,10>>
%%133> L = binary_to_list(Bin).
%%[72,101,108,108,111,0,0,0,0,10]
%%134> Str.
%%* 1: variable 'Str' is unbound
%%135> <<Str:5/binary, _/binary>> = Bin.
%%<<72,101,108,108,111,0,0,0,0,10>>
%%136> Str.
%%<<"Hello">>
%%137> binary_to_list(Str).
%%"Hello"

fetch(N) ->
  IoDevice = open_for_read(),
  case file:pread(IoDevice, file_location(N), file_location(N+1)) of
    {ok, Data} ->
      close(IoDevice),
      binary_to_list(Data);
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
  {ok, IoDevice} = file:open(?FILENAME, [write,raw,binary]),
  IoDevice.

open_for_read() ->
  {ok, IoDevice} = file:open(?FILENAME, [read,raw,binary]),
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
  <<Bin/binary, "\n":PadSize>>.

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