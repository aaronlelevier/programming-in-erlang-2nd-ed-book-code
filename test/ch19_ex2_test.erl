%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%% Create a `dets` Table that stores the counts of calls
%% to each function. With format:
%%    [{FuncName, {Line, CallCount}]
%%% @end
%%% Created : 22. Sep 2019 4:05 PM
%%%-------------------------------------------------------------------
-module(ch19_ex2_test).
-author("aaron lelevier").
-compile(export_all).
-export([]).

-include_lib("eunit/include/eunit.hrl").

%% test examples

%%%% example of a normal test
%%success_test() ->
%%  ?assert(true).
%%
%%%% example of a generator test
%%success_test_() ->
%%  fun() -> ?assert(true) end.

%% example of a failing generator test
%%fail_test_() ->
%%  fun() -> ?assert(false) end.

setup() ->
  ch19_ex2:open_dets_table().
teardown(_X) ->
  ch19_ex2:close_dets_table(),
  ch19_ex2:table_delete().

create_test_() ->
  {foreach,
    fun setup/0,
    fun teardown/1,
    [fun create_new_user/0]}.

create_new_user() ->
  Payload = default_payload(),
  io:format("LINE:~p ~p~n", [?LINE, Payload]),
  Ret = ch19_ex2:create({create, user, Payload}),
  io:format("LINE:~p ~p~n", [?LINE, Ret]),
  ?assertEqual({ok, {id, 1}}, Ret).

is_db_request_test() ->
  ?assertEqual(true, ch19_ex2:is_db_request([{db, true}])),
  ?assertEqual(false, ch19_ex2:is_db_request([{db, false}])).

response_test() ->
  Ret = ch19_ex2:response(#{a => 1}, #{b => 2}),
  ?assertEqual(#{a => 1, b => 2}, Ret).

combine_responses_test() ->
  M1 = default_payload(),
  UserId = 42,
  M2 = #{db_transaction => create, stats => success, id => UserId},
  M3 = #{next => "follow us on twitter"},

  Ret = ch19_ex2:combine_responses([M1, M2, M3]),

  #{id := RetUserId} = Ret,
  ?assertEqual(UserId, RetUserId).

new_record_test() ->
  UserData = default_user_data(),
  #{username := Username} = UserData,
  #{email := Email} = UserData,
  Id = 1,
  Ret = ch19_ex2:new_record(Id, UserData),
  ?assertEqual(
    {{email, Email},{id, Id}, {username, Username}},
    Ret).

default_payload() ->
  Data = default_user_data(),
  #{action => create, resource => user, data => Data}.

default_user_data() ->
  #{username => aaron, email => "pyaaron@gmail.com"}.