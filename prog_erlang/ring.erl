-module(ring).
-compile(export_all).

% create N processes in a ring

start(RootPid) ->
  spawn(fun() -> loop({RootPid, self()}) end).

%rpc(Pid, Request) ->
%  Pid ! {self(), Request},
%  receive
%    {Pid, Response} ->
%      Response
%  end.

loop({RootPid, Pid}) ->
  receive
    {ring, 0} ->
      io:format("Ran out of Ns at ~p~n", [self()]),
      RootPid ! {done, self()},
      loop({RootPid, Pid});
    {ring, N} ->
      _Pid = start(RootPid),
      io:format("~p started new process ~p~n", [self(), _Pid]),
      _Pid ! {ring, N-1},
      loop({RootPid, Pid});
    Any ->
      io:format("~p Received:~p~n", [self(), Any]),
      RootPid ! {ok},
      loop({RootPid, Pid})
  end.
