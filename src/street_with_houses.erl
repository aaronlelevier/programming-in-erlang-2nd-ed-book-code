-module(street_with_houses).

-export([house/4, street/2]).

house(StreetNum, City, State, Zip) ->
  {house, {street_num, StreetNum}, {city, City}, {state, State}, {zip, Zip}}.

street(House1, House2) ->
  [House1, House2].
