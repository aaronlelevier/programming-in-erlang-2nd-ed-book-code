%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2019 5:02 AM
%%%-------------------------------------------------------------------
-module(qmanager).
-author("aaron lelevier").
-compile(export_all).

-behaviour(gen_server).
-include_lib("../macros.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({check_is_prime, N}, _From, State) ->
  ?DEBUG({check_is_prime, N}),
  {Reply, NewState} = case free_workers0(State) of
    [] ->
      case worker_count0(State) of
        0 ->
          {{error, no_workers}, State};
        _N ->
          {{error, no_free_workers}, State}
      end;
    [H|_T] ->
      qworker:do_work(H),
      {{ok, received}, State#{H => busy}}
  end,
  {reply, Reply, NewState};
handle_call({reporting_for_duty, Name}, _from, State) ->
  ?DEBUG(State),
  NewState = State#{Name => free},
  ?DEBUG(NewState),
  {reply, ok, NewState};
handle_call(worker_count, _from, State) ->
  Reply = worker_count0(State),
  {reply, Reply, State};
handle_call(free_workers, _from, State) ->
  Reply = free_workers0(State),
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
receive_work({check_is_prime, N}) ->
  ?DEBUG({check_is_prime, N}),
  gen_server:call(?MODULE, {check_is_prime, N}).

% returns the count of workers
-spec worker_count() -> integer().
worker_count() ->
  ?DEBUG(worker_count),
  gen_server:call(?MODULE, worker_count).

worker_count0(State) ->
  ?DEBUG(worker_count),
  Reply = maps:size(State),
  ?DEBUG({worker_count, reply, Reply}),
  Reply.

% returns a list of free workers
-spec free_workers() -> list().
free_workers() ->
  ?DEBUG(free_workers),
  gen_server:call(?MODULE, free_workers).

%% calculate sync w/o making `gen_server:call`
free_workers0(State) ->
  ?DEBUG(free_workers),
  Reply = [
    Name || {Name, _Status} <-
      lists:filter(
        fun({_Key, Value}) -> Value =:= free end, maps:to_list(State))
  ],
  ?DEBUG({free_workers, reply, Reply}),
  Reply.

%% adds a worker to the manager's worker state
reporting_for_duty(Name) ->
  gen_server:call(?MODULE, {reporting_for_duty, Name}).

%%%===================================================================
%%% Test functions
%%%===================================================================
test() ->
  % start ETS table for storing round robin data
  round_robin:init(),

  {ok, ManagerPid} = start_link(),
  ManagerPid = whereis(qmanager),
  0 = worker_count(),
  [] = free_workers(),

  ?DEBUG("pre, error, no_workers..."),
  {error, no_workers} = receive_work({check_is_prime, 10}),
  ?DEBUG("post, error, no_workers..."),

  ?DEBUG("Adding Worker1..."),
  {ok, WorkerPid1} = qworker:start_link(round_robin:add(qworker)),

  % 1st round robin worker
  WorkerPid1 = whereis(qworker1),

  % worker is initialized as status=free
  {ok, free} = qworker:status(qworker1),

  ?DEBUG("Worker1 is free"),

  % manager now has a worker
  ?DEBUG("manager now has a worker..."),
  1 = worker_count(),
  [qworker1] = free_workers(),

  % receive work and now worker is busy
  {ok, received} = receive_work({check_is_prime, 10}),
  {ok, busy} = qworker:status(qworker1),

  % can't receive work right now because now free workers
  Ret2 = receive_work({check_is_prime, 10}),
  ?DEBUG(Ret2),
  {error, no_free_workers} = Ret2,

  ok.