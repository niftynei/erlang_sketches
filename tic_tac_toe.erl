%%% tic_tac_toe.erl
% tic tac toe scorer as a gen_server
-module(tic_tac_toe).
-behavior(gen_server).

% API
-export([new/0, move/2, score/2]).

% required by gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% new API methods
new() ->
  gen_server:start(?MODULE, [], []).

move({Player, Row, Col}, Pid) ->
  gen_server:call(Pid, {move, Player, Row, Col}).

score(Player, Pid) ->
  gen_server:call(Pid, {score, Player}).

%%% gen_server callbacks
%%%  these are required to implement gen_server
%%%  we're really only using init and handle_call
init([]) ->
  % the second value is the initial state
  {ok, {{0, 0, 0}, {0, 0, 0}, {0, 0}}}.

handle_call({move, Player, Row, Col}, _From, State) ->
  NewState = update({Player, Row, Col}, State),
  % the second value is sent back to caller
  % the third value is the new state
  {reply, ok, NewState};

handle_call({score, Player}, _From, State) ->
  _IsWin = get_score(Player, State),
  {reply, _IsWin, State}.

% Private functions
update({Player, Row, Col}, {Rows, Cols, Diags}) ->
  Val = get_val(Player),
  _RowState = update_val(Val, Row, Rows),
  _ColState = update_val(Val, Col, Cols),
  _DiagState = update_diag(Val, Row, Col, Diags),
  {_RowState, _ColState, _DiagState}.

% return true if there is a win
get_score(Player, {{R1, R2, R3}, {C1, C2, C3}, {D1, D2}}) -> 
  _Scores = [R1, R2, R3, C1, C2, C3, D1, D2],
  Wins = get_win_function(Player),
  lists:any(Wins, _Scores).

get_val(x) -> 1;
get_val(o) -> -1;
get_val(_) -> 0.

get_win_function(x) -> fun(X) -> if X >= 3 -> true; true -> false end end;
get_win_function(o) -> fun(X) -> if X =< -3 -> true; true -> false end end;
get_win_function(_) -> fun(_) -> false end.

update_val(Val, Place, {P1, P2, P3}) ->
  case Place of
    1 -> {P1 + Val, P2, P3}; 
    2 -> {P1, P2 + Val, P3};
    3 -> {P1, P2, P3 + Val};
    _ -> {P1, P2, P3}
  end.

update_diag(Val, Row, Col, {D1, D2}) ->
  if 
    Row == 1, Col == 1 -> {D1 + Val, D2};
    Row == 1, Col == 3 -> {D1, D2 + Val};
    Row == 2, Col == 2 -> {D1 + Val, D2 + Val};
    Row == 3, Col == 1 -> {D1, D2 + Val};
    Row == 3, Col == 3 -> {D1 + Val, D2};
    true -> {D1, D2}
  end.


handle_cast(_Msg, N) ->
  {noreply, N}.
handle_info(_Msg, N) ->
  {noreply, N}.
code_change(_OldVsn, N, _Other) ->
  {ok, N}.
terminate(_Reason, _N) ->
  ok.
