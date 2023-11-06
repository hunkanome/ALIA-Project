:- ensure_loaded(fct_evaluation).
:- ensure_loaded(negamax).
:- ensure_loaded(niveau1).
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
    asserta( board([[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E],[E,E,E,E,E,E]]) )  %%% create a blank board
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B),nl,
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

select_ia(P) :-
    nl,
    write('Which AI do you want to be the player '),
    write(P),
    write(' ? (random/niveau1/nmax)'),
    read(IA),
    set_ia(P,IA)
    .

set_ia(P,random) :-
    asserta( player(P, random) ), !
    .

set_ia(P, niveau1) :-
    asserta( player(P, niveau1) ), !
    .

set_ia(P, nmax) :-
    asserta( player(P, nmax) ), !
    .

set_ia(P,_):-
    nl,
    write('Please enter random, niveau1 or nmax'),
    select_ia(P)
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

play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .


%.......................................
% case : récupérer la valeur à la position (NC,NR)
%.......................................
% NC : numéro de la colonne, NR : numéro de la ligne
case(B, NC, NR, V) :-
    nth1(NC, B, C), nth1(NR, C, V).


%.......................................
% win : vérifier les conditions de victoire
%       récupérer le gagnant le cas échéant
%.......................................
win(B, D) :- winVertical(B,D),D\==e,!.
win(B, D) :- winHorizontal(B,D),D\==e,!.
win(B, D) :- winDiagonal(B,D),D\==e.
    
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
% (put disk D in square S on board B and return the resulting board B2)
%

move(B,NC,D,B2) :-
    add_disc(B,NC,D,B2)
    .


%.......................................
% game_over : vérifier si l adversaire a gagné
%             ou si la grille est pleine
%.......................................
% determines when the game is over
%
game_over(P, B) :-
    game_over2(P, B)
    .

game_over2(P, B) :-
    opponent_mark(P, D),   %%% game is over if opponent wins
    win(B, D),!
    .

game_over2(_, B) :-
    blank_mark(E),
    not(case(B,_,_,E))     %%% game is over if board is full
    .


%.......................................
% make_move
%.......................................
% requests next move from human/computer, 
% then applies that move to the given board
%

make_move(P, B) :-
    player(P, Type),

    make_move2(Type, P, B, B2),

    retract( board(_) ),
    asserta( board(B2) )
    .

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Player '),
    write(P),
    write(' move? '),
    read(S),

    blank_mark(E),
    case(B,S,_,E),
    player_mark(P, M),
    move(B, S, M, B2), !
    .

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Please select a numbered and empty column.'),nl,
    make_move2(human,P,B,B2)
    .

make_move2(random, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about his next move...'),
    player_mark(P, D),
    h_random(B, S),
    move(B,S,D,B2),

    nl,
    nl,
    write('Computer places '),
    write(D),
    write(' in column '),
    write(S),
    write('.')
    .

make_move2(nmax, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about his next move...'),nl,
    player_mark(P, D),
    (time(negamax(B, P, S))
    -> write('negamax succeded in providing a move'),nl
    ; write('negamax failed'),nl, h_random(B, S)
    ),
    move(B,S,D,B2),

    nl,
    nl,
    write('Computer places '),
    write(D),
    write(' in column '),
    write(S),
    write('.').

make_move2(niveau1, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about his next move...'),nl,
    player_mark(P, D),
    niveau1(B, P, S),
    move(B,S,D,B2),

    nl,
    nl,
    write('Computer places '),
    write(D),
    write(' in column '),
    write(S),
    write('.').

%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

moves(B,L) :- 
    not(win(B,y)),  %%% if either player already won, then there are no available moves
    not(win(B,r)),
    findall(N, is_column_N_full(B,N), L),
    L \== []
    .

% Vérifie si la colonne N est pleine
is_column_N_full(B,N) :-
    nth1(N,B,Col), /* Obtient Col, la N-ième colonne de la grille B (indexée à partir de 1) */
    last(Col,X), /* Obtient X, le dernier élément de la colonne Col */
    blank_mark(X) /* Vérifie que le dernier élément X ne correspond pas à une case vide */
    .


%.......................................
% Random heuristic 
%.......................................
% Heuristic that choses a move at random
%

h_random(B,S) :-
    moves(B,CP), /* Obtient la liste des coups possibles CP */
    length(CP,NCP), /* Obtient le nombre de coups possibles NCP */
    N is NCP + 1,
    random(1,N,X), /* Choisi un coup possible X aléatoirement */
    nth1(X,CP,S) /* Transcrit X en coup possible S */
    . 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :- 
    nl,
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2), 
    !
    .


output_winner(B) :-
    win(B,y),
    write('Y wins.'),
    !
    .

output_winner(B) :-
    win(B,r),
    write('R wins.'),
    !
    .

output_winner(_) :-
    write('No winner.')
    .

% affiche la grille
output_board(B):-
    nl,
    write('+---------------------------+'), nl,
    write('|'), output_line(B,6),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(B,5),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(B,4),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(B,3),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(B,2),
    write('|---+---+---+---+---+---+---|'), nl,
    write('|'), output_line(B,1),
    write('+---------------------------+'), nl, 
    write('  1   2   3   4   5   6   7'), nl, 
    nl.

% affiche la N-ième ligne de la grille
output_line([],_) :- nl.
output_line([C|B],N) :-
    nth1(N, C, R), write(' '), output_case(R), write(' |'), output_line(B,N).

% affiche et formate une valeur
output_case(r) :- ansi_format([bold,fg(red)],'O',[]),!.
output_case(y) :- ansi_format([bold,fg(yellow)],'O',[]),!.
output_case(e) :- write(' '),!.
output_case(D) :- write(D).


output_value(D,S,U) :-
    D == 1,
    nl,
    write('Square '),
    write(S),
    write(', utility: '),
    write(U), !
    .

output_value(_,_,_) :- 
    true
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% add disc
%.......................................
% Ajoute un jeton D, dans la colonne NC, de la grille B
% et renvoie la nouvelle grille B2

add_disc(B,NC,D,B2) :-
    blank_mark(E),
    nth1(NC,B,C), % NC ième colonne de B vers C
    nth1(NR,C,E),!, % NR ligne de la première case vide dans la colonne
    set_item(C,NR,D,C2),
    set_item(B,NC,C2,B2).    

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