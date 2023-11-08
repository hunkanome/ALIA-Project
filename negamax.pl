:-ensure_loaded(puissance4).

%.......................................
% negamax
%.......................................
negamax(Board, Player, OutMove) :-
    moves(Board, L),
    % If position is symmetric, only check the first 4 moves as moves 5,6,7 have the same scores as 3,2,1.
    (is_position_symmetric(Board)
    -> intersection(L, [1,2,3,4], L2),write('Position is symmetric ! Not cheking moves more than 4'),nl
    ; L2 = L
    ),
    better_move_ordering(L2, ListMoves),
    negamax_move_choice(Board, Player, OutMove, ListMoves, 1, -100000).

% When all moves have been explored, return the best move
negamax_move_choice(_, _, OutMove, [], OutMove, _).

% Explore a move from a position.
% If the score is better than the current best score, update the best move.
% Then explore the next move
negamax_move_choice(Board, Player, OutMove, [Move|ListMoves], BestMove, BestMoveScore) :-
    depth(Player, MaxDepth),
    negamax_move(Board, Player, Move, Score, MaxDepth, BestMoveScore),
    write('Move '),write(Move),write(' has score '),write(Score),nl,
    (Score > BestMoveScore
        -> negamax_move_choice(Board, Player, OutMove, ListMoves, Move, Score)
        ;  negamax_move_choice(Board, Player, OutMove, ListMoves, BestMove, BestMoveScore)
    ).

% Performs a move on the board and computes its score
negamax_move(Board, Player, Move, Score, Depth, Alpha) :-
    player_mark(Player, Disc),
    move(Board, Move, Disc, B2),
    next_player(Player, NextPlayer),
    Depth2 is Depth-1,
    negamax_score(B2, NextPlayer, Score, Depth2, Alpha).

% If the board is in a final state, return an overwhelming score
negamax_score(Board, Player, Score, Depth, _) :-
    Depth >= 0,
    game_over(Player, Board),
    Score is (Depth+1)*100,!.

% If the board is not in a final state, but final depth is reached, compute a score
negamax_score(Board, Player, Score, 0, _) :-
    not(game_over(Player, Board)),
    heuristic(Board, Score, Player),!.

% If the board is not in a final state, and final depth is not reached, explore the moves in the next depth
negamax_score(Board, Player, Score, Depth, Alpha) :-
    Depth > 0,
    not(game_over(Player, Board)),
    moves(Board, L),
    (is_position_symmetric(Board)
    -> intersection(L, [1,2,3,4], L2)
    ; L2 = L
    ), 
    better_move_ordering(L2, ListMoves),
    ( is_winning_move(Board, Player, ListMoves)
    -> Score is Depth*100
    ; negamax_best(Board, Player, ListMoves, S, Depth, -100000, Alpha),
    Score is -S).

% When all moves have been explored, return the best score
negamax_best(_, _, [], Score, _, Score, _).

% Explore the moves from a certain position
% if the score is better than the current best score, update the best score
% Alpha pruning : if the score is better than  alpha, stops the process and returns the best score (as it doesn't matter what the other moves are)
negamax_best(Board, Player, [Move|Moves], Score, Depth, BestScore, Alpha) :-
    negamax_move(Board, Player, Move, S, Depth, BestScore),
    ( S > BestScore
        -> (S >= -Alpha
            -> negamax_best(_, _, [], Score, _, S, _)
            ; negamax_best(Board, Player, Moves, Score, Depth, S, Alpha)
        )
        ; negamax_best(Board, Player, Moves, Score, Depth, BestScore, Alpha)
    ). 

% Checks if one of the moves is a winning move
is_winning_move(Board, Player, [Move|Moves]) :-
    player_mark(Player, Disc),
    move(Board, Move, Disc, B2),
    (game_over(Player, B2),!
    ; is_winning_move(Board, Player, Moves)).

% Computes a score for the current state of the board
heuristic(Board, Score, Player) :-
    player_mark(Player,M1),
    opponent_mark(Player,M2),
    evaluate_board(Board, M2, M1, Score).

% Returns true if the discs in the board are place symmetrically
is_position_symmetric(Board) :-
    nth1(1, Board, Col1),
    nth1(2, Board, Col2),
    nth1(3, Board, Col3),
    nth1(5, Board, Col5),
    nth1(6, Board, Col6),
    nth1(7, Board, Col7),
    same_list(Col1, Col7),
    same_list(Col2, Col6),
    same_list(Col3, Col5).
    
% Checks if two lists are the same (values and order)
same_list(L1, L2) :- maplist(=, L1, L2).

% Reorders moves in the list such that the moves in the center are explored first
better_move_ordering(List, OutList) :- centered_order(List, OutList, [4,3,5,2,6,1,7], []).

% When the goal order has been explored, return the reordered list
centered_order(_, OutList, [], OutList).

% Chekcs if the next move in the goal order is in the list of moves
% If it is, add it to the reordered list
% Then proceed with the reast of the goal list
centered_order(List, OutList, [Move|Moves], Acc) :-
    (member(Move, List)
    -> append(Acc, [Move], Acc2), centered_order(List, OutList, Moves, Acc2)
    ; centered_order(List, OutList, Moves, Acc)
    ).