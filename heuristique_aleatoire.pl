h_aleatoire(B) :-
    coups_possibles(B,CP), /* Obtient la liste des coups possibles CP */
    length(CP,NCP), /* Obtient le nombre de coups possibles NCP */
    random(1,NCP,X), /* Choisi un coup possible X aléatoirement */
    nth1(X,L,NumCol), /* Transcrit le coup possible X en numéro de colonne NumCol */
    jouer_coup(B,NumCol)
    .

coups_possibles(B,L) :- 
    findall(N, est_colonne_N_pleine(B,N), L)
    .

est_colonne_N_pleine(B,N) :-
    nth1(N,B,Col), /* Obtient Col, la N-ième colonne de la grille B (indexée à partir de 1) */
    last(Col,X), /* Obtient X, le dernier élément de la colonne Col */
    X \== e /* Vérifie que le dernier élément X ne correspond pas à une case vide */
    .

jouer_coup(_,_).