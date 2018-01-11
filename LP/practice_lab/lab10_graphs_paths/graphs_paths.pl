% conversion from neighbour-list-clause to edge-clause form.

neighbour(a, [b, d]).
neighbour(b, [a, c, d]).
neighbour(c, [b, d]).

neighbour_to_edge :-
    neighbour(Node, List),
    process(Node,List),
    fail.
neighbour_to_edge.

process(Node, [H|T]) :-
    assertz(edge(Node, H)),
    process(Node, T).
process(_, []).

% search for a path in edge-clause form.

is_edge(X, Y) :- edge(X, Y) ;
                 edge(Y, X).

% path(Source, Target, Path).

path(X, Y, Path) :- path(X, Y, [X], Path).

path(X, X, PPath, PPath).
path(X, Y, PPath, FPath) :- is_edge(X, Z),
    \+(member(Z, PPath)),
    path(Z, Y, [Z|PPath], FPath).


% restricted_path(Source, Target, RestrictionsList, Path)
% check_restrictions(RestrictionList, Path)

restricted_path(X, Y, LR, P) :- path(X, Y, P),
    check_restrictions(LR, P).

check_restrictions([], _) :- !.
check_restrictions([H|T], [H|R]) :- !, check_restrictions(T, R).
check_restrictions(T, [_|L]) :- check_restrictions(T, L).

:- dynamic sol_part/2.

optimal_path(X, Y, _) :- asserta(sol_part([], 100)),
                         path1(X, Y, [X], 1).
optimal_path(_, _, Path) :- retract(sol_part(Path, _)).

path1(X, X, Path, LPath) :- retract(sol_part(_, _)), !,
                            asserta(sol_part(Path, LPath)),
                            fail.
path1(X, Y, PPath, LPath) :- is_edge(X, Z),
                             \+(member(Z, PPath)),
                             LPath1 is LPath + 1,
                             sol_part(_, Lopt),
                             LPath1 < Lopt,
                             path1(Z, Y, [Z|PPath], LPath1).

% hamilton(NbNodes, Source, Path)

hamilton(NN, X, Path) :- NN1 is NN - 1, hamilton_path(NN1, X, X, [X], Path).

% if we visited all nodes, and arrived back to the starting node,
% we have the resulting path in the last variable.
hamilton_path(0, X, Y, Path, Path) :- is_edge(X, Y).
% if we have to visit more nodes, try to find an edge from the 
% current node, to a node Z, which has not been visited yet.
% if such a node z is found, continue the search from there.
hamilton_path(NN, X, Y, PPath, Path) :-
    NN > 0,
    NN1 is NN - 1,
    is_edge(X, Z),
    \+(member(Z, PPath)),
    hamilton_path(NN1, Z, Y, [Z|PPath], Path).

% Q9.3

% 1.
% Conversion between edge-clause and neighbour list-list rep.

% get all the edges from the knowledge base, and add them to the 
% neighbour list-list.
ec_to_nll(NLL, RLL) :- 
    retract(edge(A, B)),
    !,
    add_to_nll(NLL, A, B, NNLL),
    ec_to_nll(NNLL, RLL).
ec_to_nll(NLL, NLL).

% if there are no outgoing edges from A, create a new list for A.
add_to_nll([], A, B, [n(A, [B])]).
% we found outgoing edges for A, insert B into the list.
add_to_nll([n(A, AL)|T], A, B, [n(A, [B|AL])|T]) :- !.
% try the next element.
add_to_nll([H|T], A, B, [H|NT]) :- add_to_nll(T, A, B, NT).

% 2.
% restricted_path is inefficient, since we will continue on a path,
% even if it does not contain any of the nodes and check at the end.
% To improve the algorithm, I would generate a path to the first
% element in the restricted path, and continue from there, until no 
% nodes should be visited.

% opt_rest_path(Source, RestrictionList, PPath, Path).
% Note: the RestrictionList must contain, the Target node as the last one.

% no more restrictions, found a solution.
opt_rest_path(_, [], Path, Path).
% get the next path to the next restriction.
opt_rest_path(X, [H|T], PPath, Path) :-
    path(X, H, PPath, FPath),
    opt_rest_path(H, T, FPath, Path).

% pretty call.
opt_rest_path(X, RL, Path) :- opt_rest_path(X, RL, [X], Path).

% 3.
% make optimal path work on weighted edges.

edge(a, b, 4).
edge(a, d, 2).
edge(b, c, 3).
edge(b, d, 1).
edge(c, d, 9).

is_edge(A, B, W) :-
    edge(A, B, W) ;
    edge(B, A, W).

% solution basically consists of adding the weight to all clauses.

optimal_path_w(X, Y, _, _) :- asserta(sol_part([], 100)),
                         path_w1(X, Y, [X], 0).
optimal_path_w(_, _, Path, W) :- retract(sol_part(Path, W)).

path_w1(X, X, Path, LPath) :- retract(sol_part(_, _)), !,
                            asserta(sol_part(Path, LPath)),
                            fail.
path_w1(X, Y, PPath, LPath) :- is_edge(X, Z, W),
                             \+(member(Z, PPath)),
                             LPath1 is LPath + W,
                             sol_part(_, Lopt),
                             LPath1 < Lopt,
                             path_w1(Z, Y, [Z|PPath], LPath1).

% P9-4

% 1.
% find all closed paths via backtracking.

cycle(A, P) :- cycle(A, A, [A], P).

cycle(X, A, P, P) :- is_edge(X, A).
cycle(X, A, PP, FP) :- 
    is_edge(X, Z),
    \+(member(Z, PP)),
    cycle(Z, A, [Z|PP], FP).

% 2.
% Wolf-Goat-Cabbage problem
% encoding : [F, W, G, C]

is_goal([s, s, s, s]).

is_valid([A, A, _, A]).
is_valid([A, _, A, A]).
is_valid([A, A, A, _]).
is_valid([_, W, G, C]) :-
    W \= G, G \= C.

not_valid([F, W, G, _]) :- W = G, F \= W.
not_valid([F, _, G, C]) :- C = G, F \= G.

start_state([n, n, n, n]).

fwgc(P) :-
    start_state(S),
    fwgc(S, [S], P).

% change the bank of the object.
flip(n, s).
flip(s, n).

% just the farmer goes to the other bank.
gen_next([F, W, G, C], [FF, W, G, C]):- flip(F, FF).
% take the wolf to the other bank.
gen_next([F, F, G, C], [FF, FF, G, C]):- flip(F, FF).
% take the goat to the other bank.
gen_next([F, W, F, C], [FF, W, FF, C]):- flip(F, FF).
% take the cabbage to the other bank.
gen_next([F, W, G, F], [FF, W, G, FF]):- flip(F, FF).


% we found a solution.
fwgc(S, PP, PP) :- is_goal(S).
% check the validity of the current state,
% generate the next states, check if we have been on this state.
% if everything looks alright, continue onwards. 
fwgc(S, PP, FP) :-
    \+(not_valid(S)),
    gen_next(S, NS),
    \+(member(NS, PP)),
    fwgc(NS, [NS|PP], FP).
