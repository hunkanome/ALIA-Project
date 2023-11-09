:- ensure_loaded(fct_evaluation).
:- ensure_loaded(negamax).
:- ensure_loaded(niveau1).
:- ensure_loaded(niveau2).
:- ensure_loaded(niveau3).
:- dynamic 
    board/1,        %%% the current board
    player/2        %%% the players
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('y', 'r'). %%% determines the opposite of the given mark
inverse_mark('r', 'y').

player_mark(1, 'y').    %%% the mark for the given player
player_mark(2, 'r').    

opponent_mark(1, 'r').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'y').

blank_mark('e').        %%% the mark used in an empty square

maximizing('y').        %%% the player playing y is always trying to maximize the utility of the board position
minimizing('r').        %%% the player playing r is always trying to minimize the utility of the board position



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run :-
    hello,          %%% Display welcome message, initialize game
    play(1),        %%% Play the game starting with player 1
    goodbye         %%% Display end of game message
    . 

run :-
    goodbye
    .

hello :-
    initialize,
    nl,
    nl,
    nl,
    write('Welcome to Puissance 4.'),
    read_players,
    output_players
    .

initialize :-
    blank_mark(E),
    asserta( board([[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E]]) ),  %%% create a blank board
    open('coups.txt', write, Stream), 
    close(Stream).

goodbye :-
    board(Board),
    nl,
    nl,
    write('Game over: '),
    output_winner(Board),nl,
    write('                                 .\'\'.'),nl,
    write('       .\'\'.             *\'\'*    :_\\/_:     . '),nl,
    write('      :_\\/_:   .    .:.*_\\/_*   : /\\ :  .\'.:.\'.'),nl,
    write('  .\'\'.: /\\ : _\\(/_  \':\'* /\\ *  : \'..\'.  -=:o:=-'),nl,
    write(' :_\\/_:\'.:::. /)\\*\'\'*  .|.* \'.\'/.\'_\\(/_\'.\':\'.\''),nl,
    write(' : /\\ : :::::  \'*_\\/_* | |  -= o =- /)\\    \'  *'),nl,
    write('  \'..\'  \':::\'   * /\\ * |\'|  .\'/.\'.  \'._____'),nl,
    write('      *        __*..* |  |     :      |.   |\' .---"|'),nl,
    write('       _*   .-\'   \'-. |  |     .--\'|  ||   | _|    |'),nl,
    write('    .-\'|  _.|  |    ||   \'-__  |   |  |    ||      |'),nl,
    write('    |\' | |.    |    ||       | |   |  |    ||      |'),nl,
    write(' ___|  \'-\'     \'    ""       \'-\'   \'-.\'    \'`      |____'),nl,
    write('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'),nl,
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'), 
    !,
    run
    .

read_play_again(V) :-
    nl,
    nl,
    write('Play again (y/n)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

read_play_again(V) :-
    nl,
    nl,
    write('Please enter y or n.'),
    read_play_again(V)
    .

read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    . 


set_players(0) :- 
    select_ia(1),
    select_ia(2),!
    .

set_players(1) :-
    nl,
    write('Is human playing y or r (y moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ),!
    .

set_players(_) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .

select_ia(Player) :-
    nl,
    write('Which AI do you want to be the player '),
    write(Player),
    write(' ? (random/niveau1/niveau2/niveau3/nmax)'),
    read(IA),
    set_ia(Player,IA)
    .

set_ia(Player,random) :-
    asserta( player(Player, random) ), !.

set_ia(Player, niveau1) :-
    asserta( player(Player, niveau1) ), !.

set_ia(Player, niveau2) :-
    asserta( player(Player, niveau2) ), !.

set_ia(Player, niveau3) :-
    asserta( player(Player, niveau3) ), !.

set_ia(Player, nmax) :-
    asserta( player(Player, nmax) ), !.

set_ia(Player,_):-
    nl,
    write('Please enter one of random/niveau1/niveau2/niveau3/nmax'),
    select_ia(Player)
    .

human_playing(M) :- 
    (M == 'y' ; M == 'Y'),
    asserta( player(1, human) ),
    select_ia(2), !
    .

human_playing(M) :- 
    (M == 'r' ; M == 'R'),
    select_ia(1),
    asserta( player(2, human) ), !
    .

human_playing(_) :-
    nl,
    write('Please enter y or r.'),
    set_players(1)
    .

play(Player) :-
    board(Board), !,
    output_board(Board), !,
    not(game_over(Player, Board)), !,
    make_move(Player, Board), !,
    next_player(Player, Player2), !,
    play(Player2), !
    .


%.......................................
% case : récupérer la valeur à la position (NC,NR)
%.......................................
% NC : numéro de la colonne, NR : numéro de la ligne
case(Board, ColNum, RowNum, Value) :-
    nth1(ColNum, Board, Column), nth1(RowNum, Column, Value).


%.......................................
% win : vérifier les conditions de victoire
%       récupérer le gagnant le cas échéant
%.......................................
win(Board, Disk) :- winVertical(Board, Disk),   Disk\==e,!.
win(Board, Disk) :- winHorizontal(Board, Disk), Disk\==e,!.
win(Board, Disk) :- winDiagonal(Board, Disk),   Disk\==e.
    
% Vertical
winVertical([C|_],D) :- winVerticalCol(C,D),!.
winVertical([_|B],D) :- winVertical(B,D).
winVerticalCol([D,D,D,D,_,_],D).
winVerticalCol([_,D,D,D,D,_],D).
winVerticalCol([_,_,D,D,D,D],D).

% Horizontal
winHorizontal([C1,C2,C3,C4|_],D) :- 
    nth1(R,C1,D),nth1(R,C2,D2),nth1(R,C3,D3),nth1(R,C4,D4),
    D==D2,D2==D3,D3==D4,!.
winHorizontal([_|L],D):-
    winHorizontal(L,D).

% Both diagonals
% Diagonale bas-gauche -> haut-droite
winDiagonal([[D,_,_,_,_,_],[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_],_,_,_], D).
winDiagonal([[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_],_,_,_], D).
winDiagonal([[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_],[_,_,_,_,_,D],_,_,_], D).
winDiagonal([_,[D,_,_,_,_,_],[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_],_,_], D).
winDiagonal([_,[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_],_,_], D).
winDiagonal([_,[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_],[_,_,_,_,_,D],_,_], D).
winDiagonal([_,_,[D,_,_,_,_,_],[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_],_], D).
winDiagonal([_,_,[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_],_], D).
winDiagonal([_,_,[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_],[_,_,_,_,_,D],_], D).
winDiagonal([_,_,_,[D,_,_,_,_,_],[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_]], D).
winDiagonal([_,_,_,[_,D,_,_,_,_],[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_]], D).
winDiagonal([_,_,_,[_,_,D,_,_,_],[_,_,_,D,_,_],[_,_,_,_,D,_],[_,_,_,_,_,D]], D).

% Diagonale haut-gauche -> bas-droite
winDiagonal([[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_],[D,_,_,_,_,_],_,_,_], D).
winDiagonal([[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_],_,_,_], D).
winDiagonal([[_,_,_,_,_,D],[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_],_,_,_], D).
winDiagonal([_,[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_],[D,_,_,_,_,_],_,_], D).
winDiagonal([_,[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_],_,_], D).
winDiagonal([_,[_,_,_,_,_,D],[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_],_,_], D).
winDiagonal([_,_,[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_],[D,_,_,_,_,_],_], D).
winDiagonal([_,_,[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_],_], D).
winDiagonal([_,_,[_,_,_,_,_,D],[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_],_], D).
winDiagonal([_,_,_,[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_],[D,_,_,_,_,_]], D).
winDiagonal([_,_,_,[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_],[_,D,_,_,_,_]], D).
winDiagonal([_,_,_,[_,_,_,_,_,D],[_,_,_,_,D,_],[_,_,_,D,_,_],[_,_,D,_,_,_]], D).

% Grille réaliste
% [[r,r,y,y,e,e],[r,y,r,y,e,e],[y,r,r,y,e,e],[r,r,y,e,e,e],[y,r,y,r,e,e],[r,y,r,y,e,e],[r,r,y,y,e,e]]


%.......................................
% move
%.......................................
% applies a move on the given board
% (put disk D in column on board B and return the resulting board B2)
%

move(Board,Column,Disk,Board2) :-
    add_disk(Board,Column,Disk,Board2).


%.......................................
% game_over : vérifier si l adversaire a gagné
%             ou si la grille est pleine
%.......................................
% determines when the game is over
%
game_over(Player, Board) :-
    game_over2(Player, Board)
    .

game_over2(Player, Board) :-
    opponent_mark(Player, Disk),   %%% game is over if opponent wins
    win(Board, Disk),!
    .

game_over2(_, Board) :-
    blank_mark(E),
    not(case(Board,_,_,E))     %%% game is over if board is full
    .


%.......................................
% make_move
%.......................................
% requests next move from human/computer, 
% then applies that move to the given board
%

make_move(Player, Board) :-
    player(Player, Type),
    make_move2(Type, Player, Board, Board2),!,
    retract( board(_) ),
    asserta( board(Board2) )
    .

make_move2(human, Player, Board, Board2) :-
    nl,
    nl,
    write('Player '),
    write(Player),
    write(' move? '),
    read(Move),

    blank_mark(E),
    case(Board, Move, _, E),
    player_mark(Player, Disk),
    move(Board, Move, Disk, Board2), !
    .

make_move2(human, Player, Board, Board2) :-
    nl,
    nl,
    write('Please select a numbered and empty column.'),nl,
    make_move2(human, Player, Board, Board2)
    .

make_move2(random, Player, Board, Board2) :-
    nl,nl,
    write('RandomAI is thinking about his next move...'),
    player_mark(Player, Disk),
    
    open('coups.txt', append, Stream),
    statistics(walltime, _),
    h_random(Board, Move),
    statistics(walltime, [_ | [ExecutionTime]]),
    write(Stream, 'Execution time : '), write(Stream, ExecutionTime), write(Stream, 'ms -> '),
    write(Stream, Move), nl(Stream),
    close(Stream),

    move(Board, Move, Disk, Board2),
    nl,nl,
    write('Computer places '),write(Disk),write(' in column '),write(Move),write('.').

make_move2(nmax, Player, Board, Board2) :-
    nl,nl,
    write('NegamaxAI is thinking about his next move...'),nl,
    player_mark(Player, Disk),

    open('coups.txt', append, Stream),
    statistics(walltime, _),
    (time(negamax(Board, Player, Move))
    -> write('negamax succeded in providing a move'),nl
    ; write('negamax failed'),nl, h_random(Board, Move)
    ),
    statistics(walltime, [_ | [ExecutionTime]]),
    write(Stream, 'Execution time : '), write(Stream, ExecutionTime), write(Stream, 'ms -> '),
    write(Stream, Move), nl(Stream),
    close(Stream),

    move(Board, Move, Disk, Board2),
    nl,nl,
    write('Computer places '),write(Disk),write(' in column '),write(Move),write('.').

make_move2(niveau1, Player, Board, Board2) :-
    nl,nl,
    write('Niveau1AI is thinking about his next move...'),nl,
    player_mark(Player, Disk),

    open('coups.txt', append, Stream),
    statistics(walltime, _),
    niveau1(Board, Player, Move),
    statistics(walltime, [_ | [ExecutionTime]]),
    write(Stream, 'Execution time : '), write(Stream, ExecutionTime), write(Stream, 'ms -> '),
    write(Stream, Move), nl(Stream),
    close(Stream),

    move(Board, Move, Disk, Board2),
    nl,nl,
    write('Computer places '),write(Disk),write(' in column '),write(Move),write('.').

make_move2(niveau2, Player, Board, Board2) :-
    nl,nl,
    write('Niveau2AI is thinking about his next move...'),nl,
    player_mark(Player, Disk),

    open('coups.txt', append, Stream),
    statistics(walltime, _),
    niveau2(Board, Player, Move),
    statistics(walltime, [_ | [ExecutionTime]]),
    write(Stream, 'Execution time : '), write(Stream, ExecutionTime), write(Stream, 'ms -> '),
    write(Stream, Move), nl(Stream),
    close(Stream),

    move(Board, Move, Disk, Board2),
    nl,nl,
    write('Computer places '),write(Disk),write(' in column '),write(Move),write('.').

make_move2(niveau3, Player, Board, Board2) :-
    nl,nl,
    write('Niveau3AI is thinking about his next move...'),nl,
    player_mark(Player, Disk),

    open('coups.txt', append, Stream),
    statistics(walltime, _),
    niveau3(Board, Player, Move),
    statistics(walltime, [_ | [ExecutionTime]]),
    write(Stream, 'Execution time : '), write(Stream, ExecutionTime), write(Stream, 'ms -> '),
    write(Stream, Move), nl(Stream),
    close(Stream),

    move(Board, Move, Disk, Board2),
    nl,nl,
    write('Computer places '),write(Disk),write(' in column '),write(Move),write('.').
    


%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

moves(Board, ListMoves) :-
    not(win(Board, y)),  %%% if either player already won, then there are no available moves
    not(win(Board, r)),
    findall(ColNum, is_column_N_full(Board, ColNum), ListMoves),
    ListMoves \== []
    .


% Vérifie si la colonne N est pleine
is_column_N_full(Board, ColNum) :-
    nth1(ColNum, Board, Col), /* Obtient Col, la N-ième colonne de la grille B (indexée à partir de 1) */
    last(Col, X), /* Obtient X, le dernier élément de la colonne Col */
    blank_mark(X) /* Vérifie que le dernier élément X ne correspond pas à une case vide */
    .


%.......................................
% Random heuristic 
%.......................................
% Heuristic that choses a move at random
%

h_random(Board, Move) :-
    moves(Board, AllMoves), /* Obtient la liste des coups possibles */
    length(AllMoves, NbMoves), /* Obtient le nombre de coups possibles */
    N is NbMoves + 1,
    random(1, N, Choice), /* Choisi un coup possible aléatoirement */
    nth1(Choice, AllMoves, Move) /* Transcrit Choice en coup possible */
    . 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :- 
    nl,
    player(1, Player1),
    write('Player 1 is '),write(Player1),
    nl,
    player(2, Player2),
    write('Player 2 is '),write(Player2), !.


output_winner(Board) :-
    win(Board, y),
    write('Y wins.'), !.

output_winner(Board) :-
    win(Board, r),
    write('R wins.'), !.

output_winner(_) :-
    write('No winner.').

% affiche la grille
output_board(Board):-
    nl,
    write('+---------------------------+'), nl,
    write('|'), output_line(Board, 6),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(Board, 5),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(Board, 4),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(Board, 3),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(Board, 2),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(Board, 1),
    write('+---------------------------+'), nl, 
    write('  1   2   3   4   5   6   7'), nl, 
    nl.

% affiche la N-ième ligne de la grille
output_line([], _) :- nl.
output_line([Column|Board], RowNum) :-
    nth1(RowNum, Column, Case), write(' '), output_case(Case), write(' |'), output_line(Board,RowNum).

% affiche et formate une valeur
output_case(r) :- ansi_format([bold,fg(red)],'O',[]),!.
output_case(y) :- ansi_format([bold,fg(yellow)],'O',[]),!.
output_case(e) :- write(' '),!.
output_case(Value) :- write(Value). % should never happen


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% add disc
%.......................................
% Ajoute un jeton D, dans la colonne NC, de la grille B
% et renvoie la nouvelle grille B2

add_disk(Board, ColNum, Disk, Board2) :-
    blank_mark(E),
    nth1(ColNum, Board, Column), % ColNum ième colonne de Board vers C
    nth1(RowNum, Column, E),!, % NR ligne de la première case vide dans la colonne
    set_item(Column, RowNum, Disk, NewColumn), % Ajoute le jeton dans la colonne
    set_item(Board, ColNum, NewColumn, Board2).  % Remplace la colonne dans la grille

%.......................................
% set_item
%.......................................
% Given a list L, replace the item at position N with V
% return the new list in list L2
%

set_item(L, N, V, L2) :-
    set_item2(L, N, V, 1, L2)
    .

set_item2([], N, _, _, L2) :- 
    N == -1, 
    L2 = []
    .

set_item2( [_|T1], N, V, A, [V|T2] ) :- 
    A = N,
    A1 is N + 1,
    set_item2( T1, -1, V, A1, T2 )
    .

set_item2( [H|T1], N, V, A, [H|T2] ) :- 
    A1 is A + 1, 
    set_item2( T1, N, V, A1, T2 )
    .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%