%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2019 2:44 PM
%%%-------------------------------------------------------------------
-module(job_queue).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-include("records.hrl").
-include("../macros.hrl").

% placeholder `init` for Makefile
init0() -> ok.

%% creates a new job queue
init() ->
  #job_queue{
    job_num = 0,
    backlog = queue:new(),
    in_progress = ets:new(in_progress, []),
    done = ets:new(done, [])
  }.

%% public API

%% adds a job to the backlog
add_job(JobQueue, Job) ->
  Backlog = JobQueue#job_queue.backlog,
  NewJobNum = new_job_num(JobQueue),
  NewBacklog = queue:in({NewJobNum, Job}, Backlog),
  NewJobQueue = JobQueue#job_queue{job_num = NewJobNum, backlog = NewBacklog},
  {NewJobNum, NewJobQueue}.

work_wanted(JobQueue) ->
  case has_jobs(JobQueue) of
    false ->
      {no, JobQueue};
    true ->
      % job is removed from backlog
      {{value, {JobNum, Job}}, NewBackLog} = queue:out(JobQueue#job_queue.backlog),
      % job added to in-progress table
      ets:insert(JobQueue#job_queue.in_progress, {JobNum, Job}),
      NewJobQueue = JobQueue#job_queue{backlog = NewBackLog},
      {{JobNum, Job}, NewJobQueue}
  end.

%% moves a Job from "in-progress" to "done" ets table
job_done(JobNum, JobQueue) ->
  ets:delete(JobQueue#job_queue.in_progress, JobNum),
  ets:insert(JobQueue#job_queue.done, {JobNum}).

%% displays statistics about the JobQueue
statistics(JobQueue) -> {
  {job_num, JobQueue#job_queue.job_num},
  {backlog, JobQueue#job_queue.backlog},
  {in_progress, ets:match(JobQueue#job_queue.in_progress, '$1')},
  {done, ets:match(JobQueue#job_queue.done, '$1')}
}.

%% public API - helpers

%% looks up a job in the relative table
lookup(TableName, JobNum, JobQueue) ->
  case TableName of
    in_progress ->
      lookup1(JobQueue#job_queue.in_progress, JobNum);
    done ->
      lookup1(JobQueue#job_queue.done, JobNum);
    _ ->
      void
  end.

%% coerce ets lookup return value
lookup1(Tab, Key) ->
  case ets:lookup(Tab, Key) of
    [Item] -> Item;
    [] -> no
  end.


%% returns bool if there's jobs in the backlog
has_jobs(JobQueue) ->
  case queue:peek(JobQueue#job_queue.backlog) of
    empty ->
      false;
    _ ->
      true
  end.

%% returns the next job-number to use
new_job_num(JobQueue) -> JobQueue#job_queue.job_num + 1.

%% manager / worker

do_work({JobNum, F}) ->
  manager_loop(JobNum, F).

%% use `spawn_link` to get a message if the worker process dies
manager_loop(JobNum, F) ->
  Pid = spawn(fun() -> worker_loop() end),
  Pid ! {self(), F},
  receive
    {Pid, Result, Value} ->
      % return value w/ result and JobNum
      {JobNum, Result, Value}
  after 2000 ->
    manager_timeout
  end.

worker_loop() ->
  receive
    {From, F} ->
      try F() of
        Value ->
          From ! {self(), success, Value}
      catch _:Why ->
        From ! {self(), fail, Why}
      end
  after 2000 ->
    worker_timeout
  end.

%% ch-22 ex-3 have a worker loop, then kill it and job
%% should go back to the job queue
manager_loop3(JobQueue, F) ->
  Pid = spawn_link(fun() -> worker_loop3() end),
  Pid ! {self(), F},
%%  exit(Pid, kill),
  receive
    {'EXIT', _Pid2, _Why} ->
      add_job(JobQueue, F);
    Other ->
      {other, Other}
  after 2000 ->
    manager_loop3_timeout
  end.

worker_loop3() ->
  receive
    {From, F}=Msg ->
      ?DEBUG(Msg),
      _ = F(),
      worker_loop3()
  after 2000 ->
    worker_loop3_timeout
  end.
