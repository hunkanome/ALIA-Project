:- ensure_loaded(puissance4).

% change_max_min(+MaxOrMin, -NewMaxOrMin)
% Changes max to min and min to max
change_max_min(max, min).
change_max_min(min, max).

% minimax(+Board, +Player, -Move)
% Returns the best move for the player on the board
minimax(Board, Player, BestMove) :-
    minimax_step(max, Board, Player, 3, BestMove, _).

% minimax_step(+MinMax, +Board, -BestMove, -BestValue)
% Chooses the best possible move for the current board.
% minimax_step(_, _, 1, 0, [], -100000).
% minimax_step(_, _, 2, 0, [], 100000).
minimax_step(MinMax, Board, Player, Depth, Index, BestValue) :-
    all_possible_boards(Player, Board, AllMoves),
    best_move(MinMax, AllMoves, Player, Depth, BestMove, BestValue),
    nth1(Index, AllMoves, BestMove).


% all_possible_boards(+Player, +Board, -AllMoves)
% AllMoves will be matched with all possible moves for the current
% Board.
all_possible_boards(Player, Board, AllBoards) :-
    moves(Board, AllMoves),
    player_mark(Player, Disk),
    findall(B, (member(Move, AllMoves), move(Board, Move, Disk, B)), AllBoards).

% best_move(+MinMax, +AllMoves, -BestMove, -BestValue)
% Chooses the next move.
best_move(_, [Move | _], Player, 0, _, BestValue) :-
    player_mark(Player, Disk),
    opponent_mark(Player, Disk2),
    evaluate_board(Move, Disk, Disk2, BestValue),!.

best_move(max, [], _, _, [], -100000).
best_move(min, [], _, _, [], 100000).
best_move(MinMax, [Move | RestMoves], Player, Depth, BestMove, BestValue) :-
    player_mark(Player, Disk),
    opponent_mark(Player, Disk2),
    evaluate_board(Move, Disk, Disk2, Value),
    best_move(MinMax, RestMoves, Player, Depth, CurrentBestM, CurrentBestV),
    compare_moves(MinMax, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue).
best_move(MinMax, [Move | RestMoves], Player, Depth, BestMove, BestValue) :-
    best_move(MinMax, RestMoves, Player, Depth, CurrentBestM, CurrentBestV),
    change_max_min(MinMax, Other),
    NewDepth is Depth - 1,
    minimax_step(Other, Move, Player, NewDepth, _, BottomBestV),
    compare_moves(MinMax, Move, BottomBestV, CurrentBestM, CurrentBestV, BestMove, BestValue).


% % minimax_step(+MinMax, +Board, +Player, +Depth, -BestMove, -BestValue)
% % Returns the best move and its value for the player on the board
% minimax_step(max, Board, Player, Depth, _, BestValue) :-
%     game_over(Player, Board),
%     write('fin du board max'),
%     BestValue is (Depth+1)*10000, !.
% minimax_step(min, Board, Player, Depth, _, BestValue) :-
%     game_over(Player, Board),
%     write('fin du board min'),
%     BestValue is -(Depth+1)*10000, !.
% minimax_step(max, Board, Player, 0, _, BestValue) :-
%     not(game_over(Player, Board)),
%     player_mark(Player, Disk),
%     opponent_mark(Player, Disk2),
%     evaluate_board(Board, Disk, Disk2, BestValue), !.
% minimax_step(min, Board, Player, 0, _, BestValue) :-
%     not(game_over(Player, Board)),
%     player_mark(Player, Disk),
%     opponent_mark(Player, Disk2),
%     evaluate_board(Board, Disk2, Disk, BestValue), !.
% minimax_step(max, Board, Player, Depth, BestMove, BestValue) :-
%     Depth > 0,
%     not(game_over(Player, Board)),
%     NewDepth is Depth - 1,
%     moves(Board, AllMoves),
%     best_max_move(AllMoves, Board, Player, NewDepth, BestMove, BestValue).
% minimax_step(min, Board, Player, Depth, BestMove, BestValue) :-
%     Depth > 0,
%     not(game_over(Player, Board)),
%     NewDepth is Depth - 1,
%     moves(Board, AllMoves),
%     best_min_move(AllMoves, Board, Player, NewDepth, BestMove, BestValue).

% % best_max_move(+AllMoves, +Board, +Player, +Depth, -BestMove, -BestValue)
% % Returns the best move and its value for the player on the board
% best_max_move([], _, _, _, [], -100000). % If there is no move and we are maximizing, the value is -infinity
% best_max_move([Move|RestMoves], Board, Player, Depth, BestMove, BestValue) :-
%     player_mark(Player, Disk),
%     move(Board, Move, Disk, NewBoard),
%     next_player(Player, Next),
%     minimax_step(min, NewBoard, Next, Depth, _, Value),
%     best_max_move(RestMoves, Board, Player, Depth, CurrentBestM, CurrentBestV),
%     compare_moves(max, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue),
%     write('.').

% % best_min_move(+AllMoves, +Board, +Player, +Depth, -BestMove, -BestValue)
% % Returns the best move and its value for the player on the board
% best_min_move([], _, _, _, [], 100000). % If there is no move and we are minimizing, the value is +infinity
% best_min_move([Move|RestMoves], Board, Player, Depth, BestMove, BestValue) :-
%     player_mark(Player, Disk),
%     move(Board, Move, Disk, NewBoard),
%     next_player(Player, Next),
%     minimax_step(max, NewBoard, Next, Depth, _, Value),
%     best_min_move(RestMoves, Board, Player, Depth, CurrentBestM, CurrentBestV),
%     compare_moves(min, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue),
%     write('#').


% compare_moves(+MinMax, +MoveA, +ValueA, +MoveB, +ValueB, -BetterMove, -BetterValue)
% Chooses the move with the best value, depending on the MinMax cycle
compare_moves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compare_moves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compare_moves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.