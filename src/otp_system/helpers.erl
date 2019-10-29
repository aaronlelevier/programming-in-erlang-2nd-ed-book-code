%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2019 6:04 AM
%%%-------------------------------------------------------------------
-module(helpers).
-author("aaron lelevier").
-export([rand_atom/1]).

%% use to generate a random name for gen_servers b/c I'm getting
%% an "already started" error when starting w/ the same name
-spec rand_atom(Atom :: atom()) -> atom().
rand_atom(Atom) ->
  S = atom_to_list(Atom),
  S2 = integer_to_list(random:uniform(1000)),
  list_to_atom(S ++ "_" ++ S2).