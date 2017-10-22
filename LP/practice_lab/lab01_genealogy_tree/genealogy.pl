woman(ana).
woman(sara).
woman(ema).
woman(maria).
woman(dorina).
woman(irina).
woman(carmen).

man(andrei).
man(george).
man(alex).
man(marius).
man(sergiu).
man(mihai).

parent(maria, ana). % maria is anas parent
parent(george,ana). % george also is anas parent
parent(maria,andrei).
parent(george,andrei).
parent(marius, maria).
parent(dorina, maria).
parent(mihai, george).
parent(irina, george).
parent(mihai, carmen).
parent(irina, carmen).
parent(carment, sara).
parent(alex, sara).
parent(carmen, ema).
parent(alex, ema).

% X is Y's mother, if X is a woman and X is the parent of Y
mother(X, Y):- woman(X), parent(X, Y).

% X is Y's father, if X is a man and X is the parent of Y
father(X, Y):- man(X), parent(X, Y).

% sibling/2: X and Y siblings if they have a common parent, and they are different
sibling(X, Y):- parent(Z, X), parent(Z, Y), X \= Y.

% sister/2: X is Y' sister if they are siblings, and X is a woman
sister(X, Y):- sibling(X, Y), woman(X).

% aunt/2: X is Y's aunt if she is the sister of Z, who is parent of Y.
aunt(X, Y):- sister(X, Z), parent(Z, Y).

brother(X, Y):- sibling(X, Y), man(X).

uncle(X, Y):- brother(X, Z), parent(Z, Y).

grandmother(X, Y):- parent(X, Z), parent(Z, Y), woman(X).

grandfather(X, Y):- parent(X, Z), parent(Z, Y), man(X).

ancestor(X, Y):- parent(X, Y).
ancestor(X, Y):- parent(X, Z), ancestor(Z, Y).

