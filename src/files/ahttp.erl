%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Usage:
%%% $ make
%%% 1> Url = "https://www.commencalusa.com/".
%%% 2> ahttp:sync(Url).
%%% 2> ahttp:async(Url).
%%% @end
%%% Created : 30. Aug 2019 06:39
%%%-------------------------------------------------------------------
-module(ahttp).
-author("aaron lelevier").
-desc("ahttp ~ aaron http - module w/ example code for making http requests").
-export([start/0, sync/1, async/1, receiver/1]).


%% httpc docs reference: http://erlang.org/doc/apps/inets/http_client.html


%% must start `inets` in order to make HTTP requests

start() ->
  ssl:start(),
  application:start(inets).


%% MAIN: entrypoint for "sync" flow
%% takes a Url, fetches that HTML page, and returns a unique
%% list of Url links on the page
-spec sync(Url :: string()) -> list().

sync(Url) ->
  {ok, Response} = httpc:request(Url),
  extract_urls_from_response(Response).

extract_urls_from_response(Response) ->
  Bin = response_to_binary(Response),
  Urls = scavenge_urls:bin2urls(Bin),
  extract_urls(Urls).


%% MAIN: entrypoint for "sync" flow
%% spawns a thread that makes an async request using `httpc:request`
%% then waits for response and parses urls. Times out after 5 sec
%% if no response

-spec async(Url :: string()) -> pid().

async(Url) ->
  spawn(?MODULE, receiver, [Url]).


-spec receiver(Url::string()) -> no_return().

receiver(Url) ->
  {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
  receiver_loop(RequestId).


-spec receiver_loop(RequestId::reference()) -> no_return().

%% receives the response from an async httpc:request
receiver_loop(RequestId) ->
  receive
    {http, {RequestId, Response}} ->
      Urls = receiver_parse_response(Response),
      io:fwrite("~p logging Urls~n", [self()]),
      io:fwrite("~p~n", [Urls]);
    % catch all - if intended pattern match fails
    Response ->
      io:fwrite("Log unknown response:~p~n", [Response])
  after 5000 ->
    io:fwrite("timeout~n")
  end.

-type response() :: {tuple(), tuple(), binary()}.

-spec receiver_parse_response(Response) -> Urls when
  Response :: response(),
  Urls:: list().

receiver_parse_response(Response) ->
  {_Status, _Headers, Content} = Response,
  Urls = scavenge_urls:bin2urls(Content),
  extract_urls(Urls).



response_to_binary(Response) ->
  {Status, _Headers, Content} = Response,
  {_HttpProtocol, 200, "OK"} = Status,
  % Content will be a integer list so convert to binary
  list_to_binary(Content).


%% takes a string and extracts and returns the first url found
%% assuming the url starts with `http` and ends with `"`

-spec extract_urls(Urls::list()) -> list().

extract_urls(Urls) ->
  extract_urls(Urls, []).

extract_urls([H | T], Acc) ->
  Url = extract(H),
  case Url of
    [] ->
      extract_urls(T, Acc);
    _ ->
      extract_urls(T, [Url | Acc])
  end;
extract_urls([], Acc) ->
  unique_list(Acc).


% takes a list w/ duplicates and returns unique items only

-spec unique_list(list()) -> list().

unique_list(L) ->
  sets:to_list(sets:from_list(L)).

extract([Ha, Hb, Hc, Hd | Ta] = L) ->
  case {Ha, Hb, Hc, Hd} of
    % "http" chars as integers
    {$h, $t, $t, $p} ->
      lists:reverse(acc_til_quote(L, []));
    _ ->
      extract([Hb, Hc, Hd | Ta])
  end;
extract([_H|_T]) -> [];
extract([]) -> [].

acc_til_quote([H | T], Acc) ->
  case H of
    $" ->
      Acc;
    _ ->
      acc_til_quote(T, [H | Acc])
  end;
acc_til_quote([], Acc) ->
  lists:reverse(Acc).
