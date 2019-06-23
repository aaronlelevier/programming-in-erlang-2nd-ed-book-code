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
  config_to_tuple/1, read_config/3, char_type/1]).


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


%% reads a file with a single key/value config where the value is
%% an int and converts it to a tuple
config_to_tuple(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  Line = io:get_line(Device, ""),
  read_config(Line, [], []).


%% reads a tuple of the config key as an atom and the value as an int
read_config([], Chars, Values) ->
  {
    list_to_atom(lists:reverse(Chars)),
    list_to_integer(lists:reverse(Values))
  };
read_config(Line, Chars, Values) ->
  [H|T] = Line,
  CharType = char_type(H),
  case CharType of
    char -> read_config(T, [H|Chars], Values);
    delimiter -> read_config(T, Chars, Values);
    number -> read_config(T, Chars, [H|Values])
  end.


%% returns an atom for the type of character
char_type(CharInt) ->
  IsNumber = is_char_number(CharInt),
  IsDelimiter = is_delimiter(CharInt),
  IsChar = is_char(CharInt),
  if IsNumber ->
      number;
    IsDelimiter ->
      delimiter;
    IsChar ->
      char
  end.


is_char_number(CharInt) ->
  Zero = 48,
  Nine = 57,
  (CharInt >= Zero) and (CharInt =< Nine).


is_delimiter(CharInt) ->
  Colon = 58,
  Whitespace = 32,
  (CharInt =:= Colon) or (CharInt =:= Whitespace).


%% takes integer value of a char and returns book if a-z or A-Z
is_char(CharInt) ->
  is_lower_char(CharInt) or is_upper_char(CharInt).


%% takes integer value of a char and returns a bool if the char is a-z
%% i.e. "a" int value is 97, so true
is_lower_char(CharInt) ->
  LowerA = 97,
  LowerZ = 122,
  (CharInt >= LowerA) and (CharInt =< LowerZ).


%% takes integer value of a char and returns a bool if the char is A-Z
%% i.e. "a" int value is 97, so true
is_upper_char(CharInt) ->
  UpperA = 65,
  UpperZ = 91,
  (CharInt >= UpperA) and (CharInt =< UpperZ).
