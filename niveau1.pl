:- ensure_loaded(puissance4).

niveau1(Board, Player, Move) :-
    moves(Board, ListMoves),
    niveau1_try_move(Board, Player, 1, Move),
    member(Move, ListMoves).


niveau1_try_move(Board, _, 8, Move) :-
    h_random(Board,Move),!.
niveau1_try_move(Board, Player, Try, Move) :-
    player_mark(Player, Disc),
    move(Board, Try, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),
    Move is Try.
niveau1_try_move(Board, Player, Try, Move) :-
    NextTry is Try + 1,
    niveau1_try_move(Board, Player, NextTry, Move).
