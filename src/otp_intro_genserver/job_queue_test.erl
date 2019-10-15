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
-include("../macros.hrl").
-include("records.hrl").

% placeholder `init` for Makefile
test() ->
  ok = test_add_job_and_the_queue_has_jobs(),
  ok = test_work_wanted_ret_no_if_no_jobs_in_queue(),
  ok = test_work_wanted_ret_job_if_job_in_queue(),
  ok = test_lookup_job_that_does_not_exist_ret_no(),
  ok = test_job_done_transitions_through_each_queue_and_table(),
  ok = test_statistics_ret_tagged_tuple_with_all_data(),
  ok.

test_add_job_and_the_queue_has_jobs() ->
  JobQueue = job_queue:init(),
  Job = fun() -> 1 + 1 end,

  false = job_queue:has_jobs(JobQueue),

  {JobNum, JobQueue2} = job_queue:add_job(JobQueue, Job),

  1 = JobNum,
  JobNum = JobQueue2#job_queue.job_num,
  true = job_queue:has_jobs(JobQueue2),

  ok.

test_work_wanted_ret_no_if_no_jobs_in_queue() ->
  JobQueue = job_queue:init(),
  {no, JobQueue} = job_queue:work_wanted(JobQueue),
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

test_lookup_job_that_does_not_exist_ret_no() ->
  JobQueue = job_queue:init(),
  JobNum = 22,
  no = job_queue:lookup(in_progress, JobNum, JobQueue),
  ok.

test_job_done_transitions_through_each_queue_and_table() ->
  JobQueue = job_queue:init(),
  Job = fun() -> 1 + 1 end,
  {JobNum, JobQueue2} = job_queue:add_job(JobQueue, Job),
  {{JobNum, Job}, JobQueue3} = job_queue:work_wanted(JobQueue2),
  {JobNum, Job} = job_queue:lookup(in_progress, JobNum, JobQueue),

  true = job_queue:job_done(JobNum, JobQueue3),

  % job no longer in 'in-progress' table
  no = job_queue:lookup(in_progress, JobNum, JobQueue),
  % job now in 'done' table
  {JobNum} = job_queue:lookup(done, JobNum, JobQueue),

  ok.

test_statistics_ret_tagged_tuple_with_all_data() ->
  JobQueue = job_queue:init(),
  Job = fun() -> 1 + 1 end,
  % add jobs to backlog
  {JobNum1, JobQueue1} = job_queue:add_job(JobQueue, Job),
  {JobNum2, JobQueue2} = job_queue:add_job(JobQueue1, Job),
  {_JobNum3, JobQueue3} = job_queue:add_job(JobQueue2, Job),
  % move jobs to "in-progress"
  {{JobNum1, Job}, JobQueue4} = job_queue:work_wanted(JobQueue3),
  {{JobNum2, Job}, JobQueue5} = job_queue:work_wanted(JobQueue4),
  % move a job to "done"
  true = job_queue:job_done(JobNum1, JobQueue5),

  Ret = job_queue:statistics(JobQueue5),

  RawRet = {
    {job_num, 3},
    {backlog, JobQueue5#job_queue.backlog},
    {in_progress, [[{2, Job}]]},
    {done, [[{1}]]}
  },
  Ret = RawRet,

  ok.