%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jul 2019 05:58
%%%-------------------------------------------------------------------
-module(compile_and_run).
-author("aaron").

%% API
-export([add_dir_to_path/1]).

% takes the relative path as Dir and adds it as an absolute path to
% Erlang code search paths
add_dir_to_path(Dir) ->
  {ok, Cwd} = file:get_cwd(),
  RequestedDir = Cwd ++ "/" ++ Dir,
  InPath = lists:member(RequestedDir, code:get_path()),
  if
    InPath =:= true ->
      io:format("~p already in code path~n", [RequestedDir]);
    true ->
      code:add_pathz(RequestedDir),
      io:format("~p added to code path~n", [RequestedDir])
  end,
  ok.


