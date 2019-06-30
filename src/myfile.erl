%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2019 09:42
%%%-------------------------------------------------------------------
-module(myfile).
-author("aaron").

%% API
-export([read/1]).


read(File) ->
  case file:read_file(File) of
    {ok, Bin} ->
      Bin;
    {error, Why} ->
      throw({error, Why, "file does not exist"})
  end.