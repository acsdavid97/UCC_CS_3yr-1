% depth-first search
edge(a, b).
edge(a, d).
edge(b, c).
edge(b, d).
edge(c, d).

is_edge(X, Y) :-
    edge(X, Y) ;
    edge(Y, X).

% d_search(Start, Path)

d_search(X, _) :- df_search(X, _).
d_search(_, L) :- collect_v([], L).

df_search(X, L) :-
    asserta(vert(X)),
    edge(X, Y),
    \+(vert(Y)),
    df_search(Y, L).

collect_v(L, P) :- retract(vert(X)), !, collect_v([X|L], P).
collect_v(L, L).

% depth-first search

% do_bfs(Start, Path).

do_bfs(X, Path) :- assertz(q(X)), asserta(vert(X)), bfs(Path).

bfs(Path) :- q(X), !, expand(X), bfs(Path).
bfs(Path) :- assertz(vert(end)), collect_v([], Path).

expand(X) :- 
    edge(X, Y),
    \+(vert(Y)),
    asserta(vert(Y)),
    assertz(q(Y)),
    fail.
expand(X) :- retract(q(X)).

% best-first search (greedy search)

pos_vec(start, 0, 2, [a, d]).
pos_vec(a, 2, 0, [start, b]).
pos_vec(b, 5, 0, [a, c, end]).
pos_vec(c, 10, 0, [b, end]).
pos_vec(d, 3, 4, [start, e]).
pos_vec(e, 7, 4, [d]).
pos_vec(end, 7, 2, [b, c]).

is_target(end).

dist(N1, N2, Dist) :- 
    pos_vec(N1, X1, Y1, _),
    pos_vec(N2, X2, Y2, _),
    Dist is (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2).

order([N1|_], [N2|_]) :-
    is_target(Target),
    dist(N1, Target, Dist1),
    dist(N2, Target, Dist2),
    Dist1 < Dist2.

best([], []) :- !.
best([[Target|Rest]|_], [Target|Rest]) :- is_target(Target), !.
best([[H|T]|Rest], Best) :-
    pos_vec(H, _, _, Vec),
    expand(Vec, [H|T], Rest, Exp),
    q(Exp, SortExp, []),
    best(SortExp, Best).

expand([], _, Exp, Exp) :- !.
expand([E|R], Path, Rest, Exp) :- 
    \+(member(E, Path)), !,
    expand(R, Path, [[E|Path]|Rest], Exp).
expand([_|R], Path, Rest, Exp) :- expand(R, Path, Rest, Exp).

partition(H,[A|X],[A|Y],Z):-
    order(A,H),!, partition(H,X,Y,Z).
partition(H,[A|X],Y,[A|Z]):-partition(H,X,Y,Z).
partition(_,[],[],[]).

q([H|T],S,R):-
    partition(H,T,A,B),
    q(A,S,[H|Y]),
    q(B,Y,R).
q([],S,S).

% Q10-3

% 1.
% Perform depth-limited search.

dl_search(X, _, DM) :- dfl_search(X, _, 0, DM).
dl_search(_, L) :- collect_v([], L).

dfl_search(X, L, D, DM) :-
    D < DM,
    D1 is D + 1,
    asserta(vert(X)),
    is_edge(X, Y),
    \+(vert(Y)),
    dfl_search(Y, L, D1, DM).
