%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2019 07:14
%%%-------------------------------------------------------------------
-module(opaque_a).
-author("aaron").

%% API
-export([get_name/0]).

-opaque name() :: {First :: string(), Last:: string()}.

-spec get_name() -> name().

get_name() ->
  {"Bob", "Jones"}.