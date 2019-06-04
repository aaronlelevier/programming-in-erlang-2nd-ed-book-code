-module(tuples).
-export([get_person/0, get_first_name/1]).

%% construct a tuple
get_person() ->
  F = {firstName, aaron},
  L = {lastName, lelevier},
  {person, F, L}.

%% deconstruct a tuple
%% use "_" as the "anonymous variable" for values
%% that we don't care about
get_first_name(Person) ->
  {person, {_, FirstName}, _} = Person,
  FirstName.



