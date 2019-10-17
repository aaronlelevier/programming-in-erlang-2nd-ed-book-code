%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 6:02 AM
%%%-------------------------------------------------------------------
-module(event_handler).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

make(Name) ->
  register(Name, spawn(fun() -> my_handler(fun no_op/1) end)).

event(Name, E) -> Name ! {event, E}.

add_handler(Name, Fun) -> Name ! {add, Fun}.

my_handler(Fun) ->
  receive
    {add, Fun1} ->
      my_handler(Fun1);
    {event, E} ->
      (catch Fun(E)),
      my_handler(Fun)
  end.

no_op(_) -> void.
