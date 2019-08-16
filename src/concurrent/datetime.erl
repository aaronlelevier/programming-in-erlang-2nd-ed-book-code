%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2019 07:07
%%%-------------------------------------------------------------------
-module(datetime).
-author("aaron lelevier").

-compile(export_all).

%% API
-export([]).


-spec elapsed_time(Start, Stop) -> Datetime when
  Start :: calendar:datetime(),
  Stop :: calendar:datetime(),
  Datetime :: calendar:datetime().

%% returns the elapsed datetime
elapsed_time(Start, Stop) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = Stop,
  {{Year2, Month2, Day2}, {Hour2, Minute2, Second2}} = Start,

  {{Year - Year2, Month - Month2, Day - Day2},
    {Hour - Hour2, Minute - Minute2, Second - Second2}}.