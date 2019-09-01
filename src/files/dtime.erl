%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% datetime utils
%%% @end
%%% Created : 01. Sep 2019 06:20
%%%-------------------------------------------------------------------
-module(dtime).
-author("aaron lelevier").
-compile(export_all).
-export([]).

% placeholder `init` for Makefile
init() -> ok.

%% returns true if datetimes are within 1 minute of each other else false
-spec same_ish(Datetime1, Datetime2) -> true | false when
  Datetime1 :: calendar:datetime(),
  Datetime2 :: calendar:datetime().

same_ish(Datetime1, Datetime2) ->
  {Date1, {Hour1, Min1, _Sec1}} = Datetime1,
  {Date2, {Hour2, Min2, _Sec2}} = Datetime2,
  Date1 =:= Date2 andalso Hour1 =:= Hour2 andalso Min1 =:= Min2.