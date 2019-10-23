%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2019 5:49 AM
%%%-------------------------------------------------------------------
-module(product_handler).
-author("aaron lelevier").
-compile(export_all).
-export([]).
-behaviour(gen_event).
-include_lib("kernel/include/logger.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, terminate/2, handle_call/2]).

%% events
-export([new_product/1]).

%% gen_server behaviour
init(State) ->
  ?LOG_DEBUG("init:~p", [State]),
  {ok, State}.

handle_event({new_product, {Name, _Inventory, _Price}=Product}, State) ->
  ?LOG_INFO("new_product:~p", [Product]),
  ?LOG_DEBUG("state:~p", [State]),
  State1 = State#{ Name => Product},
  ?LOG_DEBUG("state1:~p", [State1]),
  {ok, State1};
handle_event(Other, State) ->
  ?LOG_INFO("Other:~p", [Other]),
  {ok, State}.

handle_call(_Request, State) ->
  Reply = "you called outside of an event",
  {ok, Reply, State}.

terminate(_Args, State) ->
  ?LOG_DEBUG("terminating state:~p", [State]),
  ok.

%% events
new_product({_Name, _Inventory, _Price}=Product) ->
  gen_event:notify(product_handler, {new_product, Product}).

%% tests
test() ->
  logger:set_primary_config(level, debug),
  gen_event:start_link({local, product_handler}),
  % add handler
  gen_event:add_handler(product_handler, ?MODULE, #{}),
  % send event
  ok = new_product({black_forbike_jersey, 10, 39.00}),
  ok = new_product({black_forbike_pants, 10, 99.00}),
  % delete handler
  ok = gen_event:delete_handler(product_handler, ?MODULE, []).

