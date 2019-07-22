%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2019 18:10
%%%-------------------------------------------------------------------
-module(the_rest).
-author("aaron").

%% API
-export([apply_ex/0]).

apply_ex() ->
  apply(binaries, term_to_packet, ["bob"]).


