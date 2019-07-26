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
-chapter(["8"]).

%% API
-export([type/1, next_true/1, list_files/2, show_dict_keys/1,
  dict_keys_to_list/1, search_code_loaded/1, code_loaded_most_exports/0,
  increment_map/2, single_module_func_names_count/1,
  code_loaded_func_names_count/0, merge_counts/2,
  most_common_func_name/0, unique_func_names/0]).

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
show_dict_keys([H | T]) ->
  {Key, _} = H,
  io:format("~p~n", [Key]),
  show_dict_keys(T).


%% return all keys, the first tuple value of a 2 item tuple list
%% as a list
dict_keys_to_list(L) ->
  dict_keys_to_list(L, []).
%% private
dict_keys_to_list([], L) -> L;
dict_keys_to_list([H | T], L) ->
  {Key, _} = H,
  L2 = [Key | L],
  dict_keys_to_list(T, L2).


%% returns a list of modules loaded that's filtered by the search string
search_code_loaded(SearchString) ->
  Modules = dict_keys_to_list(code:all_loaded()),
  lists:filter(
    fun(X) -> string:find(atom_to_list(X), SearchString) =/= nomatch end,
    Modules).


%% return the name of the module with the most exported functions
%% @returns {atom, integer}
code_loaded_most_exports() ->
  Modules = dict_keys_to_list(code:all_loaded()),
  code_loaded_most_exports(Modules, undefined, 0).

code_loaded_most_exports([], MaxModule, Max) -> {MaxModule, Max};
code_loaded_most_exports([Module | T], MaxModule, Max) ->
  FuncList = module_func_list(Module),
  FuncCount = length(FuncList),
  if FuncCount > Max ->
    NewMax = FuncCount,
    NewModule = Module;
    true ->
      NewMax = Max,
      NewModule = MaxModule
  end,
  code_loaded_most_exports(T, NewModule, NewMax).


%% take a map and increment the count of the key being present if it's
%% already present, if not present, then set to 1
%% @returns map
increment_map(Key, Map) ->
  try maps:get(Key, Map) of
    Val ->
      Map#{Key => Val + 1}
  catch
    error:{badkey, Key} -> Map#{Key => 1}
  end.


%% returns the list of exported functions in a module
%% @returns list of {atom, integer}
module_func_list(Module) ->
  ModuleInfo = apply(Module, module_info, []),
  [_Module, Exports | _T] = ModuleInfo,
  {exports, FuncList} = Exports,
  FuncList.


%% returns the number of times a function name is used withing a given module
%% @returns map
single_module_func_names_count(Module) ->
  FuncList = module_func_list(Module),
  map_of_key_counts(FuncList).

map_of_key_counts(L) ->
  map_of_key_counts(L, #{}).

map_of_key_counts([], Map) -> Map;
map_of_key_counts([H | T], Map) ->
  {Key, _} = H,
  Map2 = increment_map(Key, Map),
  map_of_key_counts(T, Map2).


%% returns the count of func name usage for `code:all_loaded()`
%% @returns map
code_loaded_func_names_count() ->
  code_loaded_func_names_count(code:all_loaded(), #{}).

code_loaded_func_names_count([], Map) -> Map;
code_loaded_func_names_count([H | T], Map) ->
  {Module, _} = H,
  Map2 = single_module_func_names_count(Module),
  code_loaded_func_names_count(T, merge_counts(Map, Map2)).


%% merges 2 Maps of counts so that the result is a map of the total counts
%% @returns map
merge_counts(Map1, Map2) ->
  L = maps:to_list(Map2),
  combine_counts(L, Map1).

combine_counts([], Map) -> Map;
combine_counts([H | T], Map) ->
  {Key, Value} = H,
  Map2 = Map#{Key => Value + maps:get(Key, Map, 0)},
  combine_counts(T, Map2).


%% get most common function name
%% answer... is `module_info`
%% all:code_loaded() - returns 152 modules
%% most common func name is module_info w/ 304 because each module
%% exports this function 2x w/ an arity of 0 and 1
%% @returns {atom, integer}
most_common_func_name() ->
  L = maps:to_list(code_loaded_func_names_count()),
  most_common_func_name(L).

most_common_func_name(L) ->
  most_common_func_name(L, {undefined, 0}).

most_common_func_name([], MaxFunc) -> MaxFunc;
most_common_func_name([H | T], MaxFunc) ->
  {_Func, Count} = H,
  {_MaxFuncName, MaxFuncCount} = MaxFunc,
  if
    Count > MaxFuncCount -> NewMaxFunc = H;
    true -> NewMaxFunc = MaxFunc
  end,
  most_common_func_name(T, NewMaxFunc).


%% returns a Map of unique function names from `code:all_loaded()`
%% @returns map
unique_func_names() ->
  L = maps:to_list(code_loaded_func_names_count()),
  L2 = lists:filter(fun({_, X}) -> X =:= 1 end, L),
  maps:from_list(L2).
