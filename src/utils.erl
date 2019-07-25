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
-export([type/1, next_true/1, list_files/2, show_dict_keys/1,
  dict_keys_to_list/1, search_code_loaded/1, code_loaded_most_exports/0,
  most_common_func_name/0]).

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


%% returns all keys (the first tuple value) of a given list
%% dict's are commonly output as a list of 2 item tuples
show_dict_keys([]) -> ok;
show_dict_keys([H|T]) ->
  {Key, _} = H,
  io:format("~p~n", [Key]),
  show_dict_keys(T).


%% return all keys, the first tuple value of a 2 item tuple list
%% as a list
dict_keys_to_list(L) ->
  dict_keys_to_list(L, []).
%% private
dict_keys_to_list([], L) -> L;
dict_keys_to_list([H|T], L) ->
  {Key, _} = H,
  L2 = [Key|L],
  dict_keys_to_list(T, L2).


%% returns a list of modules loaded that's filtered by the search string
search_code_loaded(SearchString) ->
  Modules = dict_keys_to_list(code:all_loaded()),
  lists:filter(
    fun(X) -> string:find(atom_to_list(X), SearchString) =/= nomatch end,
    Modules).


%% return the name of the module with the most exported functions
code_loaded_most_exports() ->
  Modules = dict_keys_to_list(code:all_loaded()),
  code_loaded_most_exports(Modules, undefined, 0).

code_loaded_most_exports([], Module, Max) -> {Module, Max};
code_loaded_most_exports([H|T], Module, Max) ->
  ModuleInfo = apply(H, module_info, []),
  [_H1,H2|_T2] = ModuleInfo,
  {exports, FuncList} = H2,
  FuncCount = length(FuncList),
  if FuncCount > Max ->
      NewMax = FuncCount,
      NewModule = H;
    true ->
      NewMax = Max,
      NewModule = Module
  end,
  code_loaded_most_exports(T, NewModule, NewMax).
