%%%-------------------------------------------------------------------
%%% @author aaron
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2019 10:50
%%%-------------------------------------------------------------------
-module(utils).
-author("aaron").

%% API
-export([type/1, next_true/1, list_files/2]).

type(X) ->
  M = #{
    alive => is_alive(),
    atom => is_atom(X),
    binary => is_binary(X),
    bitstring => is_bitstring(X),
    boolean => is_boolean(X),
    %% builtin => is_builtin(),
    float => is_float(X),
    function => is_function(X),
    integer => is_integer(X),
    list => is_list(X),
    map => is_map(X),
    number => is_number(X),
    pid => is_pid(X),
    port => is_port(X),
    %% process_alive => is_process_alive(X),
    %% record => is_record()
    reference => is_reference(X),
    tuple => is_tuple(X)
  },
  List = maps:to_list(M),
  utils:next_true(List).


next_true([]) ->
  false;
next_true([H | T]) ->
  case H of
    {Key, true} -> Key;
    {_, false} -> next_true(T)
  end.

%% lists all files in a given Dir where the name contains the SearchString
list_files(Dir, SearchString) ->
  lists:filter(
    fun(Name) -> string:find(Name, SearchString) =/= nomatch end,
    begin
      {ok, Files} = file:list_dir(Dir),
      Files
    end).