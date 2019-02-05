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
-export([getList/0]).

getList() -> lists:seq(1,5).