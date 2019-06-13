-module(list_comp).
-export([map/2]).

map(F, L) -> [F(X) || X <- L].