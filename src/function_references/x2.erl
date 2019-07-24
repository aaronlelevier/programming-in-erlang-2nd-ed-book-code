%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2019 05:53
%%%-------------------------------------------------------------------
-module(x2).
-author("aaron").

%% API
-export([double/1]).

double(L) -> lists:map(fun x1:square/1, L).