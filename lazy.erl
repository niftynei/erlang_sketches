-module(lazy).
-export([ints_from/1]).

ints_from(N) ->
  fun() ->
    [N|ints_from(N+1)]
  end.
