-module(mylists).
-export([filterEven/1, tests/0, sum/1, map/2, prepend_single/1, prepend_mult/2,
    pop_head/1, pop_mult_head/1]).

tests() ->
    [2,4] = filterEven([1,2,3,4]),
    tests_worked.

filterEven(L) -> lists:filter(fun(X) -> (X rem 2) =:= 0 end, L).


sum([H|T]) -> H + sum(T);
sum([]) -> 0.


map(F, [H|T]) -> [F(H)|map(F,T)];
map(_, []) -> [].


prepend_single(X) ->
    List = [1,2,3],
    [X|List].


prepend_mult(X,Y) ->
    List = [1,2,3],
    [X,Y|List].


pop_head(List) ->
    [H|_] = List,
    H.


pop_mult_head(List) ->
    [_,H1|_] = List,
    H1.
