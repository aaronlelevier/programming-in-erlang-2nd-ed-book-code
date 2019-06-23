%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2019 06:33
%%%-------------------------------------------------------------------
-module(ch5_exercises).
-author("aaron").

%% API
-export([readlines/1, is_char/1, is_lower_char/1, is_upper_char/1,
  config_to_tuple/1, read_config/2]).


readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
  after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.


config_to_tuple(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  Line = io:get_line(Device, ""),
  read_config(Line, []).


%% reads a string of "Chars" until runs into a non-char and converts
%% the string to an atom
read_config([], Chars) ->
  list_to_atom(lists:reverse(Chars));
read_config(Line, Chars) ->
  [H|T] = Line,
  IsChar = is_char(H),
  if IsChar =:= false ->
      read_config(T, Chars);
    true ->
      read_config(T, [H|Chars])
  end.


%% takes integer value of a char and returns book if a-z or A-Z
is_char(CharInt) ->
  is_lower_char(CharInt) or is_upper_char(CharInt).


%% takes integer value of a char and returns a bool if the char is a-z
%% i.e. "a" int value is 97, so true
is_lower_char(CharInt) ->
  (CharInt >= 97) and (CharInt =< 122).

%% takes integer value of a char and returns a bool if the char is A-Z
%% i.e. "a" int value is 97, so true
is_upper_char(CharInt) ->
  (CharInt >= 65) and (CharInt =< 91).
