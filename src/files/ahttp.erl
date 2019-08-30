%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Aug 2019 06:39
%%%-------------------------------------------------------------------
-module(ahttp).
-author("aaron lelevier").
-desc("ahttp ~ aaron http - module w/ example code for making http requests").
-compile(export_all).
-export([]).

%% httpc docs reference: http://erlang.org/doc/apps/inets/http_client.html

%% TODO: make a async flow

% placeholder `init` for Makefile
init() -> ok.

%% must start `inets` in order to make HTTP requests

start() ->
  ssl:start(),
  application:start(inets).

%% perform synchronous HTTP GET request
%% SO Answer reference: https://stackoverflow.com/a/21564092/1913888

-spec get(Url :: string()) -> Response :: binary().

get(Url) ->
  {ok, Response} = httpc:request(Url),
  {Status, _Headers, Content} = Response,
  {_HttpProtocol, 200, "OK"} = Status,
  % Content will be a integer list so convert to binary
  list_to_binary(Content).


%% takes a string and extracts and returns the first url found
%% assuming the url starts with `http` and ends with `"`

extract([Ha,Hb,Hc,Hd|Ta] = L) ->
  case {Ha,Hb,Hc,Hd} of
    % "http" chars as integers
    {104,116,116,112} ->
      lists:reverse(acc_til_quote(L, []));
    _ ->
      extract([Hb,Hc,Hd|Ta])
  end;
extract([H|T]) -> [];
extract([]) -> [].

acc_til_quote([H|T], Acc) ->
  case H of
    $" ->
      Acc;
    _ ->
      acc_til_quote(T, [H|Acc])
  end;
acc_til_quote([], Acc) ->
  lists:reverse(Acc).

%% TODO: add func now that processes a list of Urls and returns extracted Urls only