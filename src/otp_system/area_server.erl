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
-export([start_link/0, area/1, test/0, test2/0]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%% interface
%% each gen_server started will get a unique name, that is the
%% module_name+index, which comes from the round_robin ETS table
start_link() ->
  Name = round_robin:add(?MODULE),
  ?DEBUG(Name),
  gen_server:start_link({local, Name}, ?MODULE, [], []).

%% public
%% requests are sent round_robin
area(Thing) ->
  Name = round_robin:next(?MODULE),
  ?DEBUG(Name),
  gen_server:call(Name, {area, Thing}).

%% gen_server callbacks
init(State) ->
  process_flag(trap_exit, true),
  ?DEBUG("init"),
  {ok, State}.

handle_call({area, Thing}, _From, State) ->
  ?DEBUG({handle_call, self()}),
  {reply, compute_area1(Thing), State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, State) ->
  ?DEBUG({"terminatring", reason, Reason, state, State}),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

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
  49 = area({square, 7}),
  20 = area({rectongle, 4, 5}),
  % will fail (on purpose)
  area({rectangle, 4, 5}).

test2() ->
  % start event manager
  my_alarm_handler:start(),
  % init round_robin ETS table
  round_robin:init(),
  % start 2 servers
  start_link(),
  start_link(),
  % make requests
  16 = area({square, 4}),
  25 = area({square, 5}),
  36 = area({square, 6}).