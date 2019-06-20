%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jun 2019 07:35
%%%-------------------------------------------------------------------
-module(todos).
-author("aaron").

%% records docs: http://erlang.org/doc/getting_started/record_macros.html

%% how to import a `records` file
-include("records.hrl").

%% API
-export([mark_complete/1]).

%% update a single key on a record
mark_complete(#todo{} = R) ->
  R#todo{status=complete}.

%% NEXT - Maps

%% blog of examples: https://learnyousomeerlang.com/maps

