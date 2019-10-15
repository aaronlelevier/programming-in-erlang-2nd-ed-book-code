%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2019 6:41 AM
%%%-------------------------------------------------------------------
-module(job_centre_test).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-include_lib("../macros.hrl").

% placeholder `init` for Makefile
test() ->
  true = job_centre:start_link(),
  F1 = fun() -> 1 + 1 end,
  1 = job_centre:add_job(F1),

  {1, F1} = job_centre:work_wanted(),

  no = job_centre:work_wanted(),

  ok.