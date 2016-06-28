-module(ctemplate).
%cheater cheater pumpkin eater
-compile(export_all).

start() ->
  spawn(fun() -> loop([]) end).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> 
      Response
  end.

loop(X) ->
  recieve
    Any ->
      io:format("Received:~p~n", [Any]),
      loop(X)
  end.
