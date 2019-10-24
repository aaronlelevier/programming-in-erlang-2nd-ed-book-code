%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Oct 2019 5:26 AM
%%%-------------------------------------------------------------------
-module(gen_server_template).
-author("aaron lelevier").
-behavior(gen_server).

%% interface exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%% interface
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init(State) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, 'Reply', State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
