% permutation sort
perm_sort(L, R) :- perm(L, R), is_ordered(R), !.

% permutate the elments of a list
perm(L, [H|R]):-append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

is_ordered([_]).
is_ordered([H1, H2|T]):-H1 =< H2, is_ordered([H2|T]).

sel_sort(L, [M|R]) :- minimum_bwd(L, M), delete(M, L, L1), 
    write(M), writeln(L1), sel_sort(L1, R).
sel_sort([], []).

minimum_bwd([H], H).
minimum_bwd([H|T], M):-minimum_bwd(T, M), H>=M.
minimum_bwd([H|T], H):-minimum_bwd(T, M), H<M.

delete(X, [X|T], T) :- !. 
delete(X, [H|T], [H| R]) :- delete(X, T, R). 
delete(_, [], []). 

del_min(L, R) :-
    minimum_bwd(L, M),
    delete(M, L, R).

ins_sort([H|T], R) :- ins_sort(T, R1), write(H), writeln(R1), insert_ord(H, R1, R).
ins_sort([], []).

insert_ord(X, [H|T], [H|R]) :- X > H, !, insert_ord(X, T, R).
insert_ord(X, T, [X|T]).

bubble_sort(L, R) :- one_pass(L, R1, F), writeln(R1), nonvar(F), !, bubble_sort(R1, R).
bubble_sort(L, L).

one_pass([H1, H2|T], [H2|R], F):- H1>H2, !, F = 1, one_pass([H1|T], R, F).
one_pass([H1|T], [H1|R], F):-one_pass(T, R, F).
one_pass([], [] ,_).

quick_sort([H|T], R) :- partition(H, T, Sm, Lg), quick_sort(Sm, SmS),
            quick_sort(Lg, LgS), append(SmS, [H|LgS], R).

partition1(H, [X|T], [X|Sm], Lg) :- X < H, !, partition1(H, T, Sm, Lg).
partition1(H, [X|T], Sm, [X|Lg]) :- partition1(H, T, Sm, Lg).
partition1(_, [], [], []).

merge_sort(L, R) :- split(L, L1, L2), merge_sort(L1, R1), merge_sort(L2, R2),
                    merge(R1, R2, R).

split(L, L1, L2) :- length(L, Len), Len > 1, K is Len/2 splitK(L, K, L1, L2).

splitK([H|T], K, [H|L1], L2) :- K > 0, !, K1 is K-1, splitK(T, K1, L1, L2).
splitK(T, _, [], T).

merge([H1|T1], [H2|T2], [H1|R]) :- H1 < H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]) :- merge([H1|T1], T2, R).
merge([], L, L).
merge(L, [], L).

