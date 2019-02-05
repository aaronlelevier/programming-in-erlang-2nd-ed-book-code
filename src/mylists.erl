-module(mylists).
-export([filterEven/1, tests/0, sum/1, map/2]).

tests() ->
    [2,4] = filterEven([1,2,3,4]),
    tests_worked.

filterEven(L) -> lists:filter(fun(X) -> (X rem 2) =:= 0 end, L).


sum([H|T]) -> H + sum(T);
sum([]) -> 0.


map(F, [H|T]) -> [F(H)|map(F,T)];
map(_, []) -> [].
