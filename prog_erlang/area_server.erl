-module(area_server).
-export([loop/0]).

loop() ->
  receive
    {rectangle, Width, Ht} ->
      io:format("Area of rect is ~p~n", [Width * Ht]),
      loop();
    {circle, R} ->
      io:format("Area of circl is ~p~n", [3.14159 * R * R]),
      loop();
    Other ->
      io:format("I dunno what the area of a ~p is ~n", [Other]),
      loop()
  end.

