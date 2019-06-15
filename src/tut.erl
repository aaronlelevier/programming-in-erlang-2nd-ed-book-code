-module(tut).

-import(string, [len/1, concat/2, chr/2, substr/2, str/2,
  to_lower/1, to_upper/1]).

-export([main/0, atom_stuff/0, do_math/2, do_integer_math/2,
  do_floating_point_math/2, get_remainder/2, compare/2,
  compare_gt_lt/2, compare_logical/2, what_grade/1,
  case_switch/1, string_interpolation/2, tuple_stuff/0]).

main() ->
  atom_stuff().

atom_stuff() ->
  'An Atom'.

do_math(A, B) ->
  A + B.

do_integer_math(A, B) ->
  A div B.

do_floating_point_math(A, B) ->
  A / B.

get_remainder(A, B) ->
  A rem B.

compare(A, B) ->
  {
    {'=:=', A =:= B, 'value and type match'},
    {'=/=', A =/= B, 'value and type do not match'},
    {'==', A == B, 'value matches and disregard type'},
    {'/=', A /= B, 'value does not match and disregard type'}
  }.

compare_gt_lt(A, B) ->
  {
    {'<', A < B},
    {'=<', A =< B},
    {'>', A > B},
    {'>=', A >= B}
  }.

compare_logical(A, B) ->
  A and B.

%% Logical operators
%%
%% 57> true and true.
%% true
%% 58> true and false.
%% false
%% 59> true or false.
%% true
%% 60> true xor false.
%% true
%% 61> true xor true.
%% false
%% 62> not true.
%% false
%% 63> not false.
%% true

%% if/else example
preschool() -> "Go to preschool".

kindergarten() -> "Go to kindergarten".

grade_school() -> "Go to grade school".

what_grade(X) ->
  if X < 5 -> preschool();
    X == 5 -> kindergarten();
    X > 5 -> grade_school()
  end.

%% case switch example
case_switch(Lang) ->
  case Lang of
    french -> "Bonjour";
    german -> "Guten Tag"
  end.

string_interpolation(Str1, Str2) ->
  io:fwrite("String : ~p ~s\n", [Str1, Str2]).

%% finding the index of a char in a string

%% 91> string:ch
%% chars/2  chars/3  chomp/1  chr/2
%% 91> string:chr("Gary", $a).
%% 2
%% 92> string:chr("Gary", $y).
%% 4
%% 93> string:chr("Gary", $z).
%% 0
%% 94> string:chr("Gary", $g).
%% 0
%% 95> string:chr("Gary", $G).
%% 1
%% 96> string:chr("Gary", $y).
%% 4

tuple_stuff() ->
  MyData = {42, 175, 6.25},
  MyData,

  {A, B, C} = MyData,
  C,

  {D, _, _} = {A, B, C},
  D,

  MyData2 = {height, "5'11"},
  {height, Height} = MyData2,
  Height.