%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2019 05:53
%%%-------------------------------------------------------------------
-module(x1).
-author("aaron").

%% API
-export([square/1, double/1]).

square(X) -> X * X.

double(L) -> lists:map(fun square/1, L).