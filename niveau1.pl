niveau1(Board,Player,Move) :-
    moves(Board, ListMoves),
    niveau1_try_move(Board,Player,Move),
    member(Move, ListMoves),!.

niveau1_try_move(Board,Player,1) :-
    player_mark(Player, Disc),
    move(Board, 1, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),!.
niveau1_try_move(Board,Player,2) :-
    player_mark(Player, Disc),
    move(Board, 2, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),!.
niveau1_try_move(Board,Player,3) :-
    player_mark(Player, Disc),
    move(Board, 3, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),!.
niveau1_try_move(Board,Player,4) :-
    player_mark(Player, Disc),
    move(Board, 4, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),!.
niveau1_try_move(Board,Player,5) :-
    player_mark(Player, Disc),
    move(Board, 5, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),!.
niveau1_try_move(Board,Player,6) :-
    player_mark(Player, Disc),
    move(Board, 6, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),!.
niveau1_try_move(Board,Player,7) :-
    player_mark(Player, Disc),
    move(Board, 7, Disc, B2),
    next_player(Player, NextPlayer),
    game_over(NextPlayer, B2),!.
niveau1_try_move(Board,_,Move) :-
    h_random(Board,Move),!.