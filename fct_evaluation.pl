/** 
Adaptation de l'heuristique de jay@identity.pub en Prolog
Crédits : https://identity.pub/2019/10/16/minimax-connect4.html

Description de l'heuristique :
Tout d'abord, on découpe le plateau en ensembles de quatre cellules adjacentes.
Notez qu'il y a beaucoup de ces ensembles et qu'ils se chevauchent (fortement). 
Pour chaque ensemble, si un joueur est le seul à avoir des pièces dans cet ensemble, il recevra un point pour chaque pièce qu'il a dans cet ensemble. 
Cela signifie que les pièces seront comptées plusieurs fois si elles apparaissent dans plusieurs ensembles qui ne sont occupés que par ce joueur. 
Le nombre total de points est la valeur heuristique pour ce joueur.
**/

% 2D element finder
get_element(Board, Row, Col, Element) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Element).

/** Obtention d'une slice du plateau (slice = 4 cases adjacentes) **/
get_slice(Board, Row, Col, Direction, Slice) :-
    findall(Element, (
        between(0, 3, Offset), % Itération sur 4 cases adjacentes à partir d une case de départ (x=Row, y=Col)
        (
            Direction = horizontal,
            NewCol is Col + Offset,
            get_element(Board, Row, NewCol, Element)
        ;
            Direction = vertical,
            NewRow is Row + Offset,
            get_element(Board, NewRow, Col, Element)
        ;
            Direction = diagonal,
            NewRow is Row + Offset,
            NewCol is Col + Offset,
            get_element(Board, NewRow, NewCol, Element)
        )
    ), Slice). % Ajout de l élement dans la slice

% Calcul le nombre de point rapporté par une slice pour Player 1
count_occurrences(Player1, Player2, Slice, Count) :-
    include(==(Player2), Slice, Filtered2),
    length(Filtered2, Count2),
    Count2==0,                  % un joueur doit être est le seul à avoir des pièces dans cet ensemble
    include(==(Player1), Slice, Filtered),
    length(Filtered, Count).

% Calcul le score pour Player1 sans prendre en compte son adversaire
evaluate_board_player(Board, Player1, Player2, Value) :-
    findall(Count, (    
        between(1, 7, Row),     
        between(1, 6, Col),
        get_slice(Board, Row, Col, horizontal, Slice), % Trouver toutes les slices horizontales
        count_occurrences(Player1, Player2, Slice, Count)
    ;                       
        between(1, 7, Row),     
        between(1, 6, Col),
        get_slice(Board, Row, Col, vertical, Slice), % Trouver toutes les slices verticales
        count_occurrences(Player1, Player2, Slice, Count)
    ;
        between(1, 7, Row),
        between(1, 6, Col),
        get_slice(Board, Row, Col, diagonal, Slice), % Trouver toutes les slices diagonales
        count_occurrences(Player1, Player2, Slice, Count)
    ), Counts),
    sum_list(Counts, Value).

% Calcul le score final de Player1
evaluate_board(Board, Player1, Player2, Value) :- 
    evaluate_board_player(Board, Player1, Player2, Value1),
    evaluate_board_player(Board, Player2, Player1, Value2),
    Value is Value1 - Value2.

% TO DO : replacer Value par Score pcq c mieux