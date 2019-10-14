%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2019 2:46 PM
%%%-------------------------------------------------------------------
-module(job_queue_test).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-include("records.hrl").

% placeholder `init` for Makefile
test() ->
  ok = test_add_job_and_the_queue_has_jobs(),
  ok = test_work_wanted_ret_no_if_no_jobs_in_queue(),
  ok = test_work_wanted_ret_job_if_job_in_queue(),
  ok.

test_add_job_and_the_queue_has_jobs() ->
  JobQueue = job_queue:init(),
  Job = fun() -> 1 + 1 end,

  false = job_queue:has_jobs(JobQueue),

  {JobNum, JobQueue2} = job_queue:add_job(JobQueue, Job),

  1 = JobNum,
  true = job_queue:has_jobs(JobQueue2),

  ok.

test_work_wanted_ret_no_if_no_jobs_in_queue() ->
  JobQueue = job_queue:init(),
  no = job_queue:work_wanted(JobQueue),
  ok.

test_work_wanted_ret_job_if_job_in_queue() ->
  JobQueue = job_queue:init(),
  Job = fun() -> 1 + 1 end,
  {JobNum, JobQueue2} = job_queue:add_job(JobQueue, Job),

  {{JobNum, Job}, JobQueue3} = job_queue:work_wanted(JobQueue2),

  % one job from queue is now removed
  false = job_queue:has_jobs(JobQueue3),

  % job now in the in-progress table
  {JobNum, Job} = job_queue:lookup(in_progress, JobNum, JobQueue),

  ok.