%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2019 06:50
%%%-------------------------------------------------------------------
-module(demo).
-author("aaron").

%% API
-export([getList/0, start/0, test/0]).

start() -> ok.

getList() -> lists:seq(1,5).

%% tests %%

test() ->
    ok = test_getList(),
    io:fwrite("demo tests passed~n"),
    ok.

test_getList() ->
    [1,2,3,4,5] = demo:getList(),
    ok.