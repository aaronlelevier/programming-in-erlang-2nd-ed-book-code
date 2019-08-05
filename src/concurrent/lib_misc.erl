%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2019 01:29
%%%-------------------------------------------------------------------
-module(lib_misc).
-author("aaron lelevier").

%% API
-export([start/0, priority_receive/0]).

start() ->
  spawn(limb_misc, priority_receive, []).


priority_receive() ->
  receive
    {alarm, X} ->
      {alarm, X}
  after 0 ->
    receive
      Any ->
        Any
      end
  end.