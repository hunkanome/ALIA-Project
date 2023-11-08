:- ensure_loaded(puissance4).

niveau3(Board, Player, Move) :-
    moves(Board, ListMoves),
    niveau3_try_move_win(Board, Player, 1, Move),
    Move \== -1,
    member(Move, ListMoves),!.
niveau3(Board, Player, Move) :-
    moves(Board, ListMoves),
    niveau3_try_move_not_lose(Board, Player, 1, Move),
    Move \== -1,
    member(Move, ListMoves),!.
niveau3(Board, _, Move) :-
    moves(Board, ListMoves),
    h_random(Board, Move),
    member(Move, ListMoves),!.

niveau3_try_move_win(_, _, 8, -1) :- !.
niveau3_try_move_win(Board, Player, ColNum, Move) :-
    player_mark(Player, Disk),
    move(Board, ColNum, Disk, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),
    Move is ColNum,!.
niveau3_try_move_win(Board, Player, ColNum, Move) :-
    NextColNum is ColNum + 1,
    niveau3_try_move_win(Board, Player, NextColNum, Move).
    
niveau3_try_move_not_lose(_, _, 8, -1) :- !.
niveau3_try_move_not_lose(Board, Player, ColNum, Move) :-
    opponent_mark(Player, Disk),
    move(Board, ColNum, Disk, NewBoard),
    game_over(Player, NewBoard),
    Move is ColNum,!.
niveau3_try_move_not_lose(Board, Player, ColNum, Move) :-
    NextColNum is ColNum + 1,
    niveau3_try_move_win(Board, Player, NextColNum, Move).