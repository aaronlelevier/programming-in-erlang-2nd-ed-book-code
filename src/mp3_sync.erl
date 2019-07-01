%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jul 2019 07:07
%%%-------------------------------------------------------------------
-module(mp3_sync).
-author("aaron").

%% API
-export([find_sync/2]).


%% tries to find the sync point of an MP3 Binary file
%% if the header can be found 3 times in a row, then N is the sync point
find_sync(Bin, N) ->
  case is_header(N, Bin) of
    {ok, Len1, _} ->
      case is_header(N + Len1, Bin) of
        {ok, Len2, _} ->
          case is_header(N + Len2, Bin) of
            {ok, _, _} ->
              {ok, N};
            error ->
              find_sync(Bin, N + 1)
          end;
        error ->
          find_sync(Bin, N + 1)
      end;
    error ->
      find_sync(Bin, N + 1)
  end.


is_header(N, Bin) ->
  unpack_header(get_word(N, Bin)).


%% C is 32 bits. The size, 4 below, is in bits unless the type is binary,
%% then it's in binary
get_word(N, Bin) ->
  {_, <<C:4/binary, _/binary>>} = split_binary(Bin, N),
  C.


unpack_header(X) ->
  try decode_header(X)
  catch
    _:_ -> error
  end.


%% first pattern matched bitstring is a base-2 integer of eleven 1's
decode_header(<<2#11111111111:11, B:2, C:2, _D:1, E:4, F:2, G:1, Bits:9>>) ->
  Vsn = case B of
          0 -> {2, 5};
          1 -> exit(badVsn);
          2 -> 2;
          3 -> 1
        end,
  Layer = case C of
            0 -> exit(badLayer);
            1 -> 3;
            2 -> 2;
            3 -> 1
          end,
  BitRate = bitrate(Vsn, Layer, E) * 1000,
  SampleRate = samplerate(Vsn, F),
  Padding = G,
  FrameLength = framelength(Layer, BitRate, SampleRate, Padding),
  if
    FrameLength < 21 ->
      exit(frameSize);
    true ->
      {ok, FrameLength, {Layer, BitRate, SampleRate, Vsn, Bits}}
  end;
decode_header(_) ->
  exit(badHeader).


%% stubs - referenced in book but implementation not shown

bitrate(Vsn, Layer, E) -> 0.

samplerate(Vsn, F) -> 0.

%% value > 21 so `decode_header` succeeds
framelength(Layer, BitRate, SampleRate, Padding) -> 22.

