%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2019 18:59
%%%-------------------------------------------------------------------
-module(ring).
-author("aaron lelevier").

%% API
-export([init/1, loop/1]).


init(N) ->
  Id = integer_to_atom(N),
  Fun = fun(N2) ->
    Pid = spawn(?MODULE, loop, [N2]),
    io:fwrite("Pid: ~p~n", [Pid]),
    register(Id, Pid) end,
%%  register(Id, spawn(?MODULE, loop, [N])).
  for(1, N, Fun).


integer_to_atom(N) ->
  list_to_atom(integer_to_list(N)).


for(N, N, F) ->
  io:fwrite("FN: ~p~n", [N]),
  F(N);
for(I, N, F) ->
  io:fwrite("FI: ~p~n", [I]),
  F(I),
  for(I+1, N, F).


loop(N) ->
  receive
  after 0 ->
    io:fwrite("id: ~p~n", [N])
  end.