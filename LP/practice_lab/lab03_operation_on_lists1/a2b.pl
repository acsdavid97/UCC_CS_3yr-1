a2b([], []).
a2b([a|TA], [b|TB]) :-
    a2b(TA, TB).

combine1([], [], []).
combine1([H1 | T1], [H2 | T2], [H1, H2 | T]) :-
    combine1(T1, T2, T).

combine2([], [], []).
combine2([H1 | T1], [H2 | T2], [[H1, H2] | T]) :-
    combine2(T1, T2, T).

combine3([], [], []).
combine3([H1 | T1], [H2 | T2], [j(H1, H2) | T]) :-
    combine3(T1, T2, T).
