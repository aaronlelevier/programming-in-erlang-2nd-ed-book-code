%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Oct 2019 5:26 AM
%%%-------------------------------------------------------------------
-module(prime_server).
-author("aaron lelevier").
-behavior(gen_server).
-include_lib("../macros.hrl").

%% interface exports
-export([start_link/0, new_prime/1, test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%% interface
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_prime(N) ->
  % 20000 is the timeout
  gen_server:call(?MODULE, {prime, N}, 20000).

%% gen_server callbacks
init(State) ->
  process_flag(trap_exit, true),
  ?DEBUG("init"),
  {ok, State}.

handle_call({prime, N}, _From, State) ->
  {reply, make_new_prime(N), State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
  ?DEBUG("terminatring"),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% private
make_new_prime(N) ->
  if
    N > 100 ->
      alarm_handler:set_alarm(tooHot),
      Ret = lib_primes:make_prime(N),
      alarm_handler:clear_alarm(tooHot),
      Ret;
    true ->
      lib_primes:make_prime(N)
  end.

%% tests
test() ->
  % start event manager
  my_alarm_handler:start(),
  % start server
  start_link(),
  % make requests
  new_prime(10),
  new_prime(101).
