%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jul 2019 06:37
%%%-------------------------------------------------------------------
-module(storageer).
-author("aaron").

-include_lib("records.hrl").

%% TODO: module isn't working. Continue after reviewing "Message Passing"

%% API
-export([available/1, available/2, request/1, storageer/0]).

%% returns a bool if at least one unit is available
available(Storageer) ->
  units:available(Storageer#storageer.units) > 0.


%% returns a bool if the requested number of units is available
available(Storageer, Units) ->
  units:available(Storageer#storageer.units) >= Units.


request(Request) ->
  %% as a Storageer via message passing.
  %% I don't know how to message pass
  ok.


%% have a process running for each physical entity in the system.
%%  - James, Code Elixir London

storageer() ->
  receive
    {request, Request} -> process(Request);
    {my_status, Request} -> my_status(Request);
    {info} -> info(),
      storageer()
  end.


process(Request) ->
  {process, Request}.

my_status(Request) ->
  {my_status, Request}.

info() ->
  {info, get()}.
