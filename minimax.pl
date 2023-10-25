MINIMAX(Board, Depth, Player, Move, Score)
BEST(Board, Depth, Player, Moves, Move, Score)
MOVE(Board,Move, Player, Board2)
BETTER(Player, Move1, Score1, Move2, Score2, Move, Score)
BETTER(Random, Player, Move1, Score1, Move2, Score2, Move, Score)

maximizing(y).
minimizing(r). 

minimax(Board, Depth, Player, Move, Score) :- 
    Depth is Depth -1,
    moves(Board, Moves),
    !,
    best(Board,Depth,Player,Moves,Move, Score),
    !
    .

% Condition d arrêt
% Plus de coups à jouer

minimax(Board, 0, Player, Move, Score) :-
    (Player==r, Player2 is y ; Player==y, Player2 is r),
    evaluate_board(Board, Player, Player2, Score).

% only one move left
best(Board, Depth, Player, [Moves], Move, Score) :-
    move(Board,Move, Player, Board2)
    (Player==r, Player is y ; Player==y, Player is r),
    !,
    minimax(Board2, Depth, Player, _Move, Score),
    Move = Moves, !,
    output_value(Depth, Board, Score)

% if there is more than one move in the list...
best(Board, Depth, Player, [Head|Tail], Move, Score) :-
    move(Board, Head, Player, Board2)
    (Player==r, Player is y ; Player==y, Player is r),
    !,
    minimax(Board2, Depth, Player, _Move, Score1),
    best(Board, Depth, Player, Tail, Move2, Score2), 
    output_value(Depth, Head, Score1),
    better(Player, Head, Score1, Move2, Score2, Move, Score)
    .

%.......................................
% better
%.......................................

better(Player, Move1, Score1, Move2, Score2, Move, Score) :-
    maximizing(Player),                     %%% if the player is maximizing
    Score1 > Score2,                           %%% then greater is better.
    Move = Move1,
    Score = Score1,
    !
    .

better(Player, Move1, Score1, Move2, Score2, Move, Score) :-
    minimizing(Player),                     %%% if the player is maximizing
    Score1 < Score2,                           %%% then greater is better.
    Move = Move1,
    Score = Score1,
    !
    .

better(Player, Move1, Score1, Move2, Score2, Move, Score) :-
    Score1 == Score2,                          %%% if moves have equal utility,
    random_int_1n(10,R),               %%% then pick one of them at random
    better2(Random, Player, Move1, Score1, Move2, Score2, Move, Score),    
    !
    .

better(Player, Move1, Score1, Move2, Score2, Move, Score) :-        %%% otherwise, second move is better
    Move = Move2,
    Score = Score2,
    !
    .

%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(Random, Player, Move1, Score1, Move2, Score2, Move, Score) :-
    Random < 6,
    Move = Move1,
    Score = Score1, 
    !
    .

better2(Random, Player, Move1, Score1, Move2, Score2, Move, Score) :-
    Move = Move2,
    Score = Score2, 
    !
    .

output_value(Depth,Move, Score) :-
    D == 1,
    nl,
    write('Column'),
    write(Move),
    write(', utility: '),
    write(Score), !
    .

output_value(Depth,Move, Score) :- 
    true
    .

%.......................................
% returns a random integer from 1 to N
%.......................................
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .