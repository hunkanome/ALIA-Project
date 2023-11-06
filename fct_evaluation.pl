% Adaptation de l'heuristique de jay@identity.pub en Prolog
% Crédits : https://identity.pub/2019/10/16/minimax-connect4.html

% Description de l'heuristique :
% Tout d'abord, on découpe le plateau en ensembles de quatre cellules adjacentes.
% Notez qu'il y a beaucoup de ces ensembles et qu'ils se chevauchent (fortement). 
% Pour chaque ensemble, si un joueur est le seul à avoir des pièces dans cet ensemble, il recevra un point pour chaque pièce qu'il a dans cet ensemble. 
% Cela signifie que les pièces seront comptées plusieurs fois si elles apparaissent dans plusieurs ensembles qui ne sont occupés que par ce joueur. 
% Le nombre total de points est la valeur heuristique pour ce joueur.

% 2D element finder
get_element(Board, Col, Row, Element) :-
    nth1(Col, Board, RowList),
    nth1(Row, RowList, Element).

% Obtention d une slice du plateau (slice = 4 cases adjacentes)
get_slice(Board, Col, Row, Direction, Slice) :-
    findall(Element, (
        between(0, 3, Offset), % Itération sur 4 cases adjacentes à partir d une case de départ (x=Col, y=Row)
        (
            Direction = vertical,
            NewRow is Row + Offset,
            get_element(Board, Col, NewRow, Element)
        ;
            Direction = horizontal,
            NewCol is Col + Offset,
            get_element(Board, NewCol, Row, Element)
        ;
            Direction = diagonalPos,
            NewCol is Col + Offset,
            NewRow is Row + Offset,
            get_element(Board, NewCol, NewRow, Element)
        ;   
        	Direction = diagonalNeg,
            NewCol is Col + Offset,
            NewRow is Row - Offset,
            get_element(Board, NewCol, NewRow, Element)
        )
    ), Slice), % Ajout de l élement dans la slice
    length(Slice, 4).


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
        between(1, 4, Col),     
        between(1, 6, Row),
        get_slice(Board, Col, Row, horizontal, Slice), % Trouver toutes les slices horizontales
        count_occurrences(Player1, Player2, Slice, Count)
    ;                       
        between(1, 7, Col),     
        between(1, 3, Row),
        get_slice(Board, Col, Row, vertical, Slice), % Trouver toutes les slices verticales
        count_occurrences(Player1, Player2, Slice, Count)
    ;
        between(1, 4, Col),
        between(4, 6, Row),
        get_slice(Board, Col, Row, diagonalNeg, Slice), % Trouver toutes les slices diagonales pente négative
        count_occurrences(Player1, Player2, Slice, Count)
    ;
        between(1, 4, Col),
        between(1, 3, Row),
        get_slice(Board, Col, Row, diagonalPos, Slice), % Trouver toutes les slices diagonales pente positive
        count_occurrences(Player1, Player2, Slice, Count)
    ), Counts),
    sum_list(Counts, Value).

% Calcul le score final de Player1
evaluate_board(Board, Player1, Player2, Value) :- 
    evaluate_board_player(Board, Player1, Player2, Value1),
    evaluate_board_player(Board, Player2, Player1, Value2),
    Value is Value1 - Value2.

% TO DO : replacer Value par Score pcq c mieux