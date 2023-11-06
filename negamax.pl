%.......................................
% negamax
%.......................................
negamax(Board, Player, OutMove) :-
    moves(Board, ListMoves),
    negamax_move_choice(Board, Player, OutMove, ListMoves, 1, -100000).

negamax_move_choice(_, _, OutMove, [], OutMove, _).

negamax_move_choice(Board, Player, OutMove, [Move|ListMoves], BestMove, BestMoveScore) :-
    MaxDepth is 4,
    negamax_move(Board, Player, Move, Score, MaxDepth),
    write('Move '),write(Move),write(' has score '),write(Score),nl,
    (Score > BestMoveScore
        -> negamax_move_choice(Board, Player, OutMove, ListMoves, Move, Score)
        ;  negamax_move_choice(Board, Player, OutMove, ListMoves, BestMove, BestMoveScore)
    ).

negamax_move(Board, Player, Move, Score, Depth) :-
    player_mark(Player, Disc),
    move(Board, Move, Disc, B2),
    next_player(Player, NextPlayer),
    Depth2 is Depth-1,
    negamax_score(B2, NextPlayer, Score, Depth2).

negamax_score(Board, Player, Score, Depth) :-
    Depth >= 0,
    game_over(Player, Board),
    Score is (Depth+1)*100.

negamax_score(Board, Player, Score, 0) :-
    not(game_over(Player, Board)),
    heuristic(Board, Score, Player).

negamax_score(Board, Player, Score, Depth) :-
    Depth > 0,
    not(game_over(Player, Board)),
    moves(Board, ListMoves),
    ( is_winning_move(Board, Player, ListMoves)
    -> Score is Depth*100
    ; negamax_best(Board, Player, ListMoves, S, Depth, -100000),
    %findall(X, (member(Move, ListMoves), negamax_move(Board, Player, Move, X, Depth, -100000)), ListScores),
    %write(Depth),write(':'),write(ListScores),nl,
    %max_list(ListScores, S),
    Score is -S).

negamax_best(_, _, [], Score, _, Score).

negamax_best(Board, Player, [Move|Moves], Score, Depth, BestScore) :-
    negamax_move(Board, Player, Move, S, Depth),
    ( S > BestScore
        -> negamax_best(Board, Player, Moves, Score, Depth, S)
        ; negamax_best(Board, Player, Moves, Score, Depth, BestScore)
    ). 

is_winning_move(Board, Player, [Move|Moves]) :-
    player_mark(Player, Disc),
    move(Board, Move, Disc, B2),
    (game_over(Player, B2)
    ; is_winning_move(Board, Player, Moves)).

heuristic(Board, Score, Player) :-
    player_mark(Player,M1),
    opponent_mark(Player,M2),
    evaluate_board(Board, M2, M1, Score).