%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2019 6:16 AM
%%%-------------------------------------------------------------------
-module(event_handler).
-author("aaron lelevier").
-behavior(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, terminate/2, handle_call/2]).

%% gen_event callbacks
init(State) -> {ok, State}.
handle_event(_Event, State) -> {ok, State}.
handle_call(_Request, N) -> {ok, 'Reply', N}.
terminate(_Args, _State) -> ok.
