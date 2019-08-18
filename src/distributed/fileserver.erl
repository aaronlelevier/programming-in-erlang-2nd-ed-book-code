%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Aug 2019 07:06
%%%-------------------------------------------------------------------
-module(fileserver).
-author("aaron lelevier").
-desc("remote file server example").

-compile(export_all).

%% API
-export([]).

%% Types

-type filename() :: string().

%% Functions

% placeholder `init` for Makefile
init() -> ok.

%% on a separate Node, start rpc server Process and return Pid
-spec start(Node) -> Pid when
  Node :: node(),
  Pid :: pid().

start(Node) ->
  dist_demo:start(Node).

%% returns the current working directory (CWD) of the Pid
-spec get_cwd(Pid) -> {ok, Cwd} when
  Pid :: pid(),
  Cwd :: string().

get_cwd(Pid) ->
  dist_demo:rpc(Pid, file, get_cwd, []).

%% lists all files in the CWD of the Pid
-spec list_dir(Pid) -> {ok, DirList} when
  Pid :: pid(),
  DirList :: [filename()].

list_dir(Pid) ->
  dist_demo:rpc(Pid, file, list_dir, ["."]).

%% returns the file contents based on the Filename and
%% the directory that the Pid lives in
-spec read_file(Pid, Filename) -> {ok, Contents} when
  Pid :: pid(),
  Filename :: filename(),
  Contents :: binary().

read_file(Pid, Filename) ->
  dist_demo:rpc(Pid, file, read_file, [Filename]).

%% Tests
%% To run these tests, must call this in a separate
%% terminal window first, so that a 2nd Node is running
%% to test against. Run cmd:
%% `$ cd ../concurrent && export NODE=concurrent && make`

test() ->
  Pid = start(concurrent@ac),
  ok = test_get_cwd(Pid),
  ok = test_list_dir(Pid),
  ok = test_read_file(Pid),
  ok.

test_get_cwd(Pid) ->
  {ok, Cwd} = get_cwd(Pid),
  true = is_list(Cwd),
  ok.

test_list_dir(Pid) ->
  {ok, DirList} = list_dir(Pid),
  true = is_list(DirList),
  ok.

test_read_file(Pid) ->
  {ok, Contents} = read_file(Pid, "utils.erl"),
  true = is_binary(Contents),
  true = size(Contents) > 0,
  ok.
