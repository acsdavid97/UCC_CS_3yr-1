numeral(0).
numeral(succ(X)) :- numeral(X).

add(0, Y, Y).
add(succ(X), Y, succ(Z)) :-
        add(X, Y, Z).

greater_than(X, 0) :- numeral(X), X \= 0.
greater_than(X, Y) :- greater_than(succ(X), succ(Y).

swap(leaf(X), leaf(X)).
swap(tree(X, Y), T) :- T = tree(Y1, X1),
                            swap(Y, Y1),
                            swap(X, X1).
