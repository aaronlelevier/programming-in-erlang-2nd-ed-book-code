%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc CH-19 ex-1 - make a tips DB about tips for good Erlang Programs
%%%
%%% @end
%%% Created : 28. Sep 2019 5:40 AM
%%%-------------------------------------------------------------------
-module(tips).
-author("aaron lelevier").
-compile(export_all).
-export([]).

-include_lib("stdlib/include/qlc.hrl").

%% set - unique username, emails can be reused
-record(user, {username, email}).

%% set - unique flag name
-record(flag, {name, color}).

%% duplicate-bag - instance of user abuse
-record(abuse, {user, flag, url}).

%% bag - url is the unique key
-record(tip, {url, title, desc, user, tags}).

%% set - use to tag tips
-record(tag, {name, color}).

%% macros

-define(TABLES, [user, flag, abuse, tip, tag]).

%% init and insert initial data

init() ->
  ok = mnesia:start(),
  ok = try_insert_init_data(fun reset_tables/0),
  ok.

try_insert_init_data(Fun) ->
  try_insert_init_data(Fun, 1).

try_insert_init_data(Fun, Counter) ->
  case Fun() of
    {atomic, ok} ->
      io:format("Tables populated~n"),
      io:format("~p~n", [select_all(user)]),
      io:format("~p~n", [select_all(flag)]),
      io:format("~p~n", [select_all(abuse)]),
      ok;
    {aborted, Error} ->
      io:format("Error:~p~n", [Error]),
      if Counter < 5 ->
        timer:sleep(500),
        try_insert_init_data(Fun, Counter + 1);
        true ->
          Error
      end
  end.

%% create tables

%% do this once
create_tables() ->
  mnesia:create_schema([node()]),
  mnesia:start(),

  mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
  mnesia:create_table(flag, [{attributes, record_info(fields, flag)}]),
  mnesia:create_table(abuse, [{attributes, record_info(fields, abuse)}, {type, bag}]),
  mnesia:create_table(tip, [{attributes, record_info(fields, tip)}]),
  mnesia:create_table(tag, [{attributes, record_info(fields, tag)}]),

  mnesia:stop().


reset_tables() ->
  [mnesia:clear_table(X) || X <- ?TABLES],
  F = fun() ->
    lists:foreach(fun mnesia:write/1, example_data())
      end,
  mnesia:transaction(F).

example_data() ->
  % users
  User1 = #user{username = aaron, email = "aaron@gmail.com"},
  User2 = #user{username = bob, email = "bob@gmail.com"},
  User3 = #user{username = john, email = "john@gmail.com"},
  % flags
  Flag1 = #flag{name = prohibited, color = red},
  Flag2 = #flag{name = spam, color = yellow},
  % abuse
  Abuse1 = #abuse{user = User2#user.username, flag = Flag1#flag.name},
  Abuse2 = #abuse{user = User2#user.username, flag = Flag2#flag.name},
  Abuse3 = #abuse{user = User3#user.username, flag = Flag2#flag.name},

  [% user: username, email
    User1, User2, User3,
    % flag: name, color
    Flag1, Flag2,
%%    % abuse: user, flag
    Abuse1, Abuse2, Abuse3
%%    % tip: url, title, desc, user, tags
%%    {tip},
%%    % tag: name, color
%%    {tag}
  ].

%% queries

select_all(Table) ->
  tables:do(qlc:q([X || X <- mnesia:table(Table)])).

select_abusive_users() ->
  tables:do(qlc:q([
    {U#user.username, U#user.email} ||
    U <- mnesia:table(user),
    A <- mnesia:table(abuse),
    U#user.username =:= A#abuse.user])).

count_abuses() ->
  L = tables:do(qlc:q([
    {A#abuse.user, 1} ||
    A <- mnesia:table(abuse)
  ])),
  combine_list_of_counts(L).

%% combine list of counts

%% test case func
combine_list_of_counts() ->
  L = [{a, 1}, {b, 1}, {a, 1}, {b, 3}, {c,1}],
  combine_list_of_counts(L).

-spec combine_list_of_counts(L :: list()) -> #{any() => integer()}.

%% func for general usage
combine_list_of_counts(L) ->
  combine_list_of_counts(L, #{}).

%% func that does the actual work
combine_list_of_counts([], M) -> M;
combine_list_of_counts([H|T], M) ->
  {Key, Val} = H,
  Val2 = maps:get(Key, M, 0),
  combine_list_of_counts(T, M#{Key => Val + Val2}).

%% TODO: do the same w/ `qlc:fold`