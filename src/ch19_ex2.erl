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


-ifdef(debug_flag).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n", [?MODULE, ?LINE, X])). -else.
-define(DEBUG(X), void).
-endif.

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

create({create, Resource, Payload}) ->
  io:format("~p~n", [Payload]),
  #{data := Data} = Payload,
  #{username := Username} = Data,
  case dets:lookup(Resource, Username) of
    [] ->
      UserId = dets:insert(Resource, {Username, Data}),
      {ok, {id, UserId}};
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
combine_responses([H|T], Acc) ->
  Acc2 = maps:merge(Acc, H),
  combine_responses(T, Acc2);
combine_responses([], Acc) ->
  Acc.

open_dets_table() ->
  File = atom_to_list(?MODULE),
  io:format("dets opened:~p~n", [File]),
  Bool = filelib:is_file(File),
  case dets:open_file(?MODULE, [{file, File}]) of
    {ok, Ref} ->
      case Bool of
        true -> void;
        false ->
          ok = dets:insert(Ref, {free, 1})
      end,
      true;
    {error, Reason} ->
      io:format("cannot open dets table~n"),
      exit({eDetsOpen, File, Reason})
  end.


close_dets_table() ->
  dets:close(?MODULE).