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
-module(ch19_ex2).
-author("aaron lelevier").
-compile(export_all).
-export([]).


-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n", [?MODULE, ?LINE, X])).

% placeholder `init` for Makefile
init() -> ok.

start(Request, Opts) ->
  spawn(?MODULE, run, [Request, Opts]).

run(Request, Opts) ->
  % runs initial business rules
  BizResp = biz(Request),

  % check the options if a DB call is needed
  DbResp = case is_db_request(Opts) of
             true ->
               db(Request);
             false ->
               #{}
           end,
  ok.
%%  % return final response
%%  Response = combine_responses([BizResp, DbResp])
%%  response(Response).

biz(Req) -> Req.

is_db_request(Opts) -> lists:member({db, true}, Opts).

db(Request) ->
  open_dets_table(),
  case Request of
    {create, _Resource, _Data} ->
      create(Request);
    _ ->
      throw({error, unkown_request})
  end,
  close_dets_table().

create({create, user, Payload}) ->
  io:format("~p~n", [Payload]),
  #{data := UserData} = Payload,
  #{username := Username} = UserData,
  Key = {user, Username},
  case table_lookup(Key) of
    [] ->
      [{free, Free} | _] = table_lookup(free),
      NewRecord = new_record(Free, UserData),
      ok = table_insert({Key, NewRecord}),
      table_insert({free, Free + 1}),
      {ok, {id, Free}};
    _ ->
      throw({error, username_must_be_unique})
  end;
create(Other) ->
  ?DEBUG(Other),
  {ok, {id, -1}}.

response(BizResp, DbResp) ->
  combine_responses([BizResp, DbResp]).

combine_responses(Responses) ->
  ?DEBUG(Responses),
  combine_responses(Responses, #{}).
combine_responses([H | T], Acc) ->
  Acc2 = maps:merge(Acc, H),
  combine_responses(T, Acc2);
combine_responses([], Acc) ->
  Acc.

new_record(Id, Data) ->
  Data2 = Data#{id => Id},
  L = maps:to_list(Data2),
  list_to_tuple(L).

%% table functions

%% opens DETS table and initializes with entry `{free,1}` if it's a new table
open_dets_table() ->
  File = atom_to_list(?MODULE),
  io:format("dets opened:~p~n", [File]),
  Bool = filelib:is_file(File),
  case dets:open_file(?MODULE, [{file, File}]) of
    {ok, _Ref} ->
      case Bool of
        true -> void;
        false ->
          ok = table_insert({free, 1})
      end,
      true;
    {error, Reason} ->
      io:format("cannot open dets table~n"),
      exit({eDetsOpen, File, Reason})
  end.

close_dets_table() ->
  dets:close(?MODULE).

table_lookup(Key) ->
  dets:lookup(?MODULE, Key).

table_insert(Data) ->
  dets:insert(?MODULE, Data).

table_delete_all_objects() ->
  dets:delete_all_objects(?MODULE).

table_delete() ->
  File = atom_to_list(?MODULE),
  file:delete(File).
