:- ensure_loaded(puissance4).
% MINIMAX(Board, Depth, Player, Move, Score)
% BEST(Board, Depth, Player, Moves, Move, Score)
% MOVE(Board,Move, Player, Board2)
% BETTER(Player, Move1, Score1, Move2, Score2, Move, Score)
% BETTER(Random, Player, Move1, Score1, Move2, Score2, Move, Score)

% Condition d arrêt
% Plus de coups à jouer

minimax(Board, 0, Player, Move, Score) :-
    next_player(Player, Player2),
    evaluate_board(Board, Player, Player2, Score),!.

minimax(Board, Depth, Player, Move, Score) :- 
    moves(Board, Moves),
    !,
    Depth2 is Depth-1,
    best(Board,Depth2,Player,Moves,Move, Score),
    !
    .

% only one move left
best(Board, Depth, Player, [Move], Move, Score) :-
    move(Board,Move, Player, Board2),
    next_player(Player, Player2),
    !,
    minimax(Board2, Depth, Player2, _Move, Score),
    !.

% if there is more than one move in the list...
best(Board, Depth, Player, [Head|Tail], Move, Score) :-
    move(Board, Head, Player, Board2),
    next_player(Player, Player2),
    !,
    minimax(Board2, Depth, Player2, _Move, Score1),
    best(Board, Depth, Player2, Tail, Move2, Score2), 
    better(Player2, Head, Score1, Move2, Score2, Move, Score)
    .

%.......................................
% better
%.......................................

better(Player, Move1, Score1, Move2, Score2, Move, Score) :-
    player_mark(Player, M),
    maximizing(M),                     %%% if the player is maximizing
    (Score1 > Score2 %%% then greatest is better.
        -> Score = Score1, Move = Move1
        ; Score = Score2, Move = Move2
    ),
    !
    .

better(Player, Move1, Score1, Move2, Score2, Move, Score) :-
    player_mark(Player, M),
    minimizing(M),                     %%% if the player is minimizing
    (Score1 < Score2 %%% then lowest is better.
        -> Score = Score1, Move = Move1
        ; Score = Score2, Move = Move2
    ),
    !
    .

% if the scores are equal, then the first move is better
better(Player, Move1, Score, Move2, Score, Move1, Score).

%.......................................
% returns a random integer from 1 to N
%.......................................
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .