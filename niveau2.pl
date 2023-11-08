:- ensure_loaded(puissance4).

niveau2(Board, Player, Move) :-
    moves(Board, ListMoves),
    niveau2_try_move(Board, Player, 1, Move),
    member(Move, ListMoves).


niveau2_try_move(Board, _, 8, Move) :-
    h_random(Board, Move),!.
niveau2_try_move(Board, Player, ColNum, Move) :-
    opponent_mark(Player, Disk),
    move(Board, ColNum, Disk, NewBoard),
    game_over(Player, NewBoard),
    Move is ColNum,!.
niveau2_try_move(Board, Player, ColNum, Move) :-
    NextColNum is ColNum + 1,
    niveau2_try_move(Board, Player, NextColNum, Move).