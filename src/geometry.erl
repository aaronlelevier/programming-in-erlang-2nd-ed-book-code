-module(geometry).

-export([test/0, area/1]).

test() ->
  12 = area({rectangle, 3, 4}),
  36 = area({square, 6}),
  216.0 = area({triangle, 24, 30, 18}),
  ok.


area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side;
area({triangle, Side1, Side2, Side3}) ->
  P = (Side1 + Side2 + Side3) / 2,
  math:sqrt(P*(P-Side1)*(P-Side2)*(P-Side3));
area({circle, Radius}) ->
  Area = math:pi() * math:pow(Radius, 2),
  Perimeter = 2 * math:pi() * Radius,
  {{area, Area}, {perimeter, Perimeter}};
area({right_triangle, A, B}) ->
  (A*B) / 2.