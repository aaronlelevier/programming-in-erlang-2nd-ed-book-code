%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2019 5:57 AM
%%%-------------------------------------------------------------------
-module(lb_server).
-author("aaron lelevier").
-behavior(gen_server).

%% interface exports
-export([start_link/0, worker_count/0, get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%% debugging
-include_lib("../macros.hrl").
-compile(export_all).

%% interface
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

worker_count() ->
  gen_server:call(?MODULE, worker_count).

get_status() ->
  gen_server:call(?MODULE, get_status).

register(WorkerName) ->
  gen_server:call(?MODULE, {register, WorkerName}).

%% gen_server callbacks
init([]) ->
  State = #{
    workers => #{}
  },
  {ok, State}.

%% returns the worker count
handle_call(worker_count, _From, State) ->
  ?DEBUG(State),
  #{ workers := Workers } = State,
  WorkerCount = maps:size(Workers),
  {reply, WorkerCount, State};
%% replies what the current state is of the lb_server
handle_call(get_status, _From, State) -> {reply, State, State};
%% registers a worker to the LB
handle_call({register, WorkerName}, _From, State) ->
  #{workers := Workers} = State,
  Workers2 = Workers#{WorkerName => 0},
  NewState = #{workers => Workers2},
  {reply, {registered, WorkerName}, NewState}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
