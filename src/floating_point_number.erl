-module(floating_point_number).
-export([int_divide_by_two/1, float_divide_by_two/1, remainder/2]).

%% div for integer division
int_divide_by_two(X) ->
  X div 2.

%% float division uses "/"
float_divide_by_two(X) ->
  X / 2.

%% remainder returned
%% ex: 5 rem 3 = 2.
remainder(X, Y) ->
  X rem Y.