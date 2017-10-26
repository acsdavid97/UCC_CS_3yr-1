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

quick_sort([H|T], R) :- partition1(H, T, Sm, Lg), quick_sort(Sm, SmS),
            quick_sort(Lg, LgS), append(SmS, [H|LgS], R).
quick_sort([], []).

partition1(H, [X|T], [X|Sm], Lg) :- X < H, !, partition1(H, T, Sm, Lg).
partition1(H, [X|T], Sm, [X|Lg]) :- partition1(H, T, Sm, Lg).
partition1(_, [], [], []).

merge_sort(L, R) :- split(L, L1, L2), merge_sort(L1, R1), merge_sort(L2, R2),
                    merge(R1, R2, R).

split(L, L1, L2) :- length(L, Len), Len > 1, K is Len/2, splitK(L, K, L1, L2).

splitK([H|T], K, [H|L1], L2) :- K > 0, !, K1 is K-1, splitK(T, K1, L1, L2).
splitK(T, _, [], T).

merge([H1|T1], [H2|T2], [H1|R]) :- H1 < H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]) :- merge([H1|T1], T2, R).
merge([], L, L).
merge(L, [], L).

% q5.1 quiz exercises
% extract just the head from the list
extract_elem([H|T], H, T).
% extract an element, which is not the head, and let prolog do the magic :)
extract_elem([H|T], E, [H|R]) :- 
    extract_elem(T, E, R).

% maximum of a list
maximum_bwd([H], H).
maximum_bwd([H|T], M):-maximum_bwd(T, M), H<M.
maximum_bwd([H|T], H):-maximum_bwd(T, M), H>=M.

% q5-2.
%selection sort maximum
sel_sort_max(L, [M|R]) :- maximum_bwd(L, M), delete(M, L, L1), sel_sort_max(L1, R).
sel_sort_max([], []).

% we need to reverse the list due the maximum elements are put to the beginning 
% of the list. The efficiency of the algorith is the same as for the traditional
% selection sort: O(N^2) since the reverse is just O(N)
sel_sort_max_pretty(L, R) :- 
    sel_sort_max(L, RR),
    reverse(RR, R).

% q5-3.
% insertion sort with forward implementation, there is no need to go backwards
% on the list, so the forward implementation has a better constant than the
% backwards one, to be more precise it performs half the calls/exits
ins_sort_fwd([H|T], Acc,  R) :- insert_ord(H, Acc, AccR), ins_sort_fwd(T, AccR, R).
% list is empty, sorted list is in Acc
ins_sort_fwd([], Acc, Acc).

% pretty call of the ins_sort_fwd
ins_sort_fwd_pretty(L, R) :-
    ins_sort_fwd(L, [], R).

% q5-4
% does K passes on the input array L
bubble_sort_fixed(L, 0, L) :- !.
bubble_sort_fixed(L, K, R) :- 
    one_pass(L, R1, _), 
    K1 is K - 1,
    bubble_sort_fixed(R1, K1, R).

% pretty call to the bubble_sort_fixed predicate
bubble_sort_fixed_pretty(L, R) :-
    length(L, Len),
    bubble_sort_fixed(L, Len, R).

% p5-1
% sort ascii characters
quick_sort_char([H|T], R) :- partition1_char(H, T, Sm, Lg), quick_sort_char(Sm, SmS),
            quick_sort_char(Lg, LgS), append(SmS, [H|LgS], R).
quick_sort_char([], []).

partition1_char(H, [X|T], [X|Sm], Lg) :- 
    char_code(X, XC),
    char_code(H, HC),
    XC < HC,
    !,
    partition1_char(H, T, Sm, Lg).
partition1_char(H, [X|T], Sm, [X|Lg]) :- partition1_char(H, T, Sm, Lg).
partition1_char(_, [], [], []).

sort_chars(L, R) :-
    quick_sort_char(L, R). 
