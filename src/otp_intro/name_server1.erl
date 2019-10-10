%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2019 5:47 AM
%%%-------------------------------------------------------------------
-module(name_server1).
-author("aaron lelevier").
-compile(export_all).
-export([]).
%% when doing this pattern, imported functions aren't resolved...
%% ex: `rpc` below
-include_lib("macros.hrl").
-import(?SERVER, [rpc/2]).

init0() -> ok.

%% client routines
add(Name, Place) -> rpc(name_server, {add, Name, Place}).
find(Name) -> rpc(name_server, {find, Name}).

%% callbacks
init() -> dict:new().

handle({add, Name, Place}, Dict) -> {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) -> {dict:find(Name, Dict), Dict}.