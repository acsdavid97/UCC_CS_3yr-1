append1([], L, L).
append1([H|T], L, [H|R]) :- append1(T, L, R).

appendr([H|T], L, [H|R]) :- append1(T, L, R).
appendr([], L, L).

delete(X, [X|T], T).
delete(X, [H|T], [H| R]) :- delete(X, T, R).
delete(_, [], []).

delete_all(X, [X|T], R) :- delete_all(X, T, R).
delete_all(X, [H|T], [H| R]) :- 
    X \= H,
    delete_all(X, T, R).
delete_all(_, [], []).

% appends three lists together
append3([], [], L3, L3).
append3([], [H|T], L3, [H|R]) :- append3([], T, L3, R).
append3([H|T], L2, L3, [H|R]) :- append3(T, L2, L3, R).

% probably faster append, since uses the builtin append function
append33(L1, L2, L3, R) :- 
    append(L1, L2, A),
    append(A, L3, R).

% adds element E to the beginning of the list L 
add_element(E, L, [E|L]).

% calculates the sum of elements in the list.
sum([], 0).
sum([H|T], R) :- 
    sum(T, R1),
    R is H + R1.

% separate even and odd numbers in different lists
separate_parity([], [], []).
separate_parity([H|T], [H|E], O) :- 
    P is mod(H, 2),
    P = 0,
    separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]) :- 
    P is mod(H, 2),
    P = 1,
    separate_parity(T, E, O).

% we remove all the occurrence of the head and pass the result recursively
remove_duplicates([], []).
remove_duplicates([H|T], [H|R]) :- 
    delete_all(H, T, R1),
    remove_duplicates(R1, R).

% replace all occurrences of K with NewK. 
replace_all(_, _, [], []).
replace_all(K, NewK, [K|T], [NewK|R]) :- 
    replace_all(K, NewK, T, R).
replace_all(K, NewK, [X|T], [X|R]) :- 
    X \= K,
    replace_all(K, NewK, T, R).

% drop the k-th elements of the list.
% in order to do this we need to use a helper function and count down to 0.
drop_k(L, K, R) :- drop_k_helper(L, K, K, R).
drop_k_helper([], _, _, []).
drop_k_helper([H|T], 0, K, R) :-
    drop_k_helper(T, K, K, R).
drop_k_helper([H|T], K1, K, [H|R]) :-
    K1 > 0,
    K2 is K1 - 1,
    drop_k_helper(T, K2, K, R).

