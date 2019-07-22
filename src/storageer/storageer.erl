%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jul 2019 06:37
%%%-------------------------------------------------------------------
-module(storageer).
-author("aaron").

-include_lib("records.hrl").

%% API
-export([available/1, available/2]).

%% returns a bool if at least one unit is available
available(Storageer) ->
  units:available(Storageer#storageer.units) > 0.


%% returns a bool if the requested number of units is available
available(Storageer, Units) ->
  units:available(Storageer#storageer.units) >= Units.