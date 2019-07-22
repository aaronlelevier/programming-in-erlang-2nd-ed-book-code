%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jul 2019 06:25
%%%-------------------------------------------------------------------
-module(units).
-author("aaron").

-include_lib("records.hrl").

%% API
-export([available/1]).

%% returns the number of available units
available(Units) ->
  Units#units.total - Units#units.in_use.
