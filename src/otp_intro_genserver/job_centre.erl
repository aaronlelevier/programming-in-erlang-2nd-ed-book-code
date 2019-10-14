%%%-------------------------------------------------------------------
%%% @author some name <me@hostname.local>
%%% @copyright (C) 2013, some name
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2013 by some name <me@hostname.local>
%%%-------------------------------------------------------------------
-module(job_centre).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include_lib("../otp_intro/macros.hrl").

%%%===================================================================
%%% job_queue
%%%===================================================================

-record(job_queue, {job_num, backlog, in_progress, done}).

init_job_queue() ->
  #job_queue{
    job_num = 0,
    backlog = queue:new(),
    in_progress = ets:new(in_progress, []),
    done = ets:new(done, [])
  }.

new_job_num(JobQueue) -> JobQueue#job_queue.job_num + 1.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

%% placeholder init
init0() -> ok.

start_link() ->
  {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  true.

%% adds a new job to the queue
add_job(F) -> gen_server:call(?MODULE, {add_job, F}).

%% called when a worker starts work on a job
work_wanted() -> gen_server:call(?MODULE, work_wanted).

%% called when a worker finishes a job
job_done(JobNum) -> gen_server:call(?MODULE, {job_done, JobNum}).

%% get statistics about the job queues
statistics() -> gen_server:call(?MODULE, statistics).

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
init([]) ->
  {ok, init_job_queue()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% adds a Job to the Queue. If the Queue is empty, start with
%% JobNumber 1, else increment the Job number
handle_call({add_job, F}, _From, JobQueue) ->
  Backlog = JobQueue#job_queue.backlog,
  NewJobNum = new_job_num(JobQueue),
  NewBacklog = queue:in({NewJobNum, F}, Backlog),
  NewJobQueue = JobQueue#job_queue{job_num = NewJobNum, backlog = NewBacklog},
  {reply, NewJobNum, NewJobQueue};
handle_call(work_wanted, _From, JobQueue) ->
  {Reply, NewJobQueue2} = case queue:out(JobQueue#job_queue.backlog) of
                            {empty, _Q} ->
                              {no, JobQueue};
                            {{value, Item}, NewBackLog} ->
                              InProgress = JobQueue#job_queue.in_progress,
                              NewInProgress = queue:in(Item, InProgress),
                              NewJobQueue = JobQueue#job_queue{
                                backlog = NewBackLog, in_progress = NewInProgress},
                              {Item, NewJobQueue}
                          end,
  {reply, Reply, NewJobQueue2};
handle_call({job_done, JobNum}, _From, JobQueue) ->
  {reply, JobNum, JobQueue};
handle_call(statistics, _From, JobQueue) ->
  Reply = {
    {job_num, JobQueue#job_queue.job_num},
    {backlog, JobQueue#job_queue.backlog},
    {in_progress, JobQueue#job_queue.in_progress},
    {done, JobQueue#job_queue.done}
  },
  {reply, Reply, JobQueue}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
