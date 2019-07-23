%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% CH-8 The Rest of Sequential Erlang
%%% @end
%%% Created : 03. Jul 2019 18:10
%%%-------------------------------------------------------------------
-module(the_rest).
-author("aaron").

%% shows up in the_rest:module_info() - attributes
-vsn("0.1.0").

%% user-defined attributes
-chapter("8").

%% API
-export([apply_ex/0, sum/1, block/1]).
-import(utils, [type/1]).

%% use to export all functions - use case for debugging only
%%-compile(export_all).

apply_ex() ->
  apply(binaries, term_to_packet, ["bob"]).

%% have two fun w/ same name but 2nd fun is an auxiliary fun
sum(L) ->
  io:format("~p is of type ~p~n", [L, type(L)]),
  sum(L, 0).

sum([], N) -> N;
sum([H | T], N) ->
  sum(T, H + N).

%% use block expressions when:
%% "you need to have multiple expressions but only one is allowed"
%% SO answer - https://stackoverflow.com/a/20132544/1913888
block(X) ->
  [begin
     Y = X * 2,
     Y + 1
   end || X <- [1, 2, 3]].
