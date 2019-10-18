%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%% Created : 18. Oct 2019 6:06 AM
%%%-------------------------------------------------------------------
-module(my_alarm_handler).
-author("aaron lelevier").
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("../macros.hrl").

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% macros
-define(SERVER, ?MODULE).

%% gen_server behaviour
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init(Args) ->
	?DEBUG({"init", args, Args}),
	{ok, 0}.
handle_call(_Request, _From, State) -> {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
