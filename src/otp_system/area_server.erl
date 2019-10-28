%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Oct 2019 6:03 AM
%%%-------------------------------------------------------------------
-module(area_server).
-author("aaron lelevier").
-include_lib("../macros.hrl").

%% interface exports
-export([start_link/0, area/1, test/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3, compute_area/1]).

%% interface
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

area(Thing) ->
  gen_server:call(?MODULE, {area, Thing}).

%% gen_server callbacks
init(State) ->
  process_flag(trap_exit, true),
  ?DEBUG("init"),
  {ok, State}.

handle_call({area, Thing}, _From, State) ->
  {reply, compute_area1(Thing), State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
  ?DEBUG("terminatring"),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% public
compute_area(Thing) ->
  ?DEBUG({compute_area, Thing}),
  gen_server:call(?MODULE, {area, Thing}).

%% private
compute_area1({square, N}) ->
  ?DEBUG({square, N}),
  Ret = N * N,
  alarm_handler:set_alarm(computeWorked),
  Ret;
% deliberate error to misspell "rectangle"
compute_area1({rectongle, H, W}) ->
  ?DEBUG({rectongle, H, W}),
  H * W.

%% tests

%% area_server:test().
test() ->
  % start event manager
  my_alarm_handler:start(),
  % start server
  start_link(),
  % make requests
  49 = compute_area({square, 7}),
  20 = compute_area({rectongle, 4, 5}),
  % will fail (on purpose)
  compute_area({rectangle, 4, 5}).

