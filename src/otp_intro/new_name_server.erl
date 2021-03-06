%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2019 5:47 AM
%%%-------------------------------------------------------------------
-module(new_name_server).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-include_lib("macros.hrl").
-import(?SERVER, [rpc/2]).

init0() -> ok.

%% client routines
add(Name, Place) -> rpc(name_server, {add, Name, Place}).
find(Name) -> rpc(name_server, {find, Name}).
all_names() -> rpc(name_server, allNames).

%% callbacks
init() -> dict:new().

%% for the return value, the 1st item is the response, and
%% the 2nd item is the new "State"
handle({add, Name, Place}, Dict) -> {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) -> {dict:find(Name, Dict), Dict};
handle(allNames, Dict) -> {dict:fetch_keys(Dict), Dict}.