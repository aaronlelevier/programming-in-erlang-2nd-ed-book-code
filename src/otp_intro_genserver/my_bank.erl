%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2019 6:19 AM
%%%-------------------------------------------------------------------
-module(my_bank).
-author("aaron lelevier").
-compile(export_all).
%% used by compiler to generate warnings if not all callbacks are defined
-behaviour(gen_server).

%% macros

-define(SERVER, my_bank).

%%%%%% interface functions %%%%%%

new_account(Who) -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).
stop() -> gen_server:call(?MODULE, stop).

%%%%%% gen_server callback functions %%%%%%

%% starts the gen_server
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% first call made by the gen_server
init([]) -> {ok, ets:new(?MODULE, [])}.

handle_call({new, Who}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] ->
              ets:insert(Tab, {Who, 0}),
              {welcome, Who};
            [_] ->
              {Who, already_a_customer}
          end,
  {reply, Reply, Tab};
handle_call({add, Who, Amount}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> not_a_customer;
            [{Who, Balance}] ->
              NewBalance = Balance + Amount,
              ets:insert(Tab, {Who, NewBalance}),
              {thanks, Who, your_balance_is, NewBalance}
          end,
  {reply, Reply, Tab};
handle_call({remove, Who, Amount}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> not_a_customer;
            [{Who, Balance}] when Amount =< Balance ->
              NewBalance = Balance - Amount,
              ets:insert(Tab, {Who, NewBalance}),
              {thanks, Who, your_balance_is, NewBalance};
            [{Who, Balance}] ->
              {sorry, Who, you_only_have, Balance, in_the_bank}
          end,
  {reply, Reply, Tab};
handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
