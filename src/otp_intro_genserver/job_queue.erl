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