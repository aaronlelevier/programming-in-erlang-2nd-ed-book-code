%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2019 10:31
%%%-------------------------------------------------------------------
-module(ch15_exercises).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

cpu() ->
  os:cmd("sysctl -n machdep.cpu.brand_string").

cpu_info() ->
  Cpu = cpu(),
  [Name, Name2, Processor, _, _, Speed] = string:split(Cpu, " ", all),
  {name, Name ++ " " ++ Name2, processor, Processor, speed, Speed}.

%% trims "foo\n" to "foo"
trim_newline(Str) ->
  string:trim(Str).

%% same as above, maybe some other benefits, but not sure..
trim_newline1(Str) ->
  re:replace(Str, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).
