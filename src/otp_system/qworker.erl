%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2019 5:27 AM
%%%-------------------------------------------------------------------
-module(qworker).
-author("aaron lelevier").
-compile(export_all).
-include_lib("../macros.hrl").

-behaviour(gen_server).

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
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  InitState = #{status => free},
  {ok, InitState}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(do_work, _From, State) ->
  #{status := Status} = State,
  case Status of
    free ->
      NewState = #{status => busy},
      {reply, {ok, busy}, NewState};
    busy ->
      {reply, {error, still_doing_work}, State}
  end;
handle_call(finish, _From, _State) ->
  NewState = #{status => free},
  {reply, {ok, free}, NewState};
handle_call(status, _From, State) ->
  #{status := Status} = State,
  {reply, {ok, Status}, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% changes status from `free` to `busy` and vice versa
-spec do_work(Name :: atom()) ->
  {ok, busy} | {error, still_doing_work}.
do_work(Name) ->
  gen_server:call(Name, do_work).

%% will finish any work if doing work and report `free`
-spec finish(Name :: atom()) ->
  {ok, free}.
finish(Name) ->
  gen_server:call(Name, finish).

%% worker reports status
-spec status(Name :: atom()) -> {ok, Status :: free | busy}.
status(Name) ->
  gen_server:call(Name, status).

%%%===================================================================
%%% Test functions
%%%===================================================================
test() ->
  {ok, _Pid} = start_link(),
  {ok, busy} = do_work(?MODULE),

  % can't send work because worker is busy
  {error, still_doing_work} = do_work(?MODULE),

  % get status, then tell to finish, and status is updated
  {ok, busy} = status(?MODULE),
  {ok, free} = finish(?MODULE),
  {ok, free} = status(?MODULE),

  % do_work sets the worker back to busy
  {ok, busy} = do_work(?MODULE),
  {ok, busy} = status(?MODULE).
