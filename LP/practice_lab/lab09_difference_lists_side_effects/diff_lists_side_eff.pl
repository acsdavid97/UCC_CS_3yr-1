% difference lists

tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))). 

add(X, LS, LE, RS, RE) :- RS = LS, LE = [X|RE].

% result is an empty list, (unify the beggining and the end of the partial
% results list).
inorder_dl(nil, L, L).
inorder_dl(t(K, L, R), LS, LE) :- 
% get lists for left/right subtree.
    inorder_dl(L, LSL, LEL),
    inorder_dl(R, LSR, LER),
% start of the final list is the start of the left list.
    LS = LSL,
% put the key at the end of the L list (or before the start of the right list).
    LEL = [K|LSR],
% the end of the final list is the end of the right list.
    LE = LER.

inorder_dl1(nil, L, L).
inorder_dl1(t(K, L, R), LS, LE) :- inorder_dl1(L, LS, [K|LT]), 
                                   inorder_dl1(R, LT, LE).

preorder_dl(nil, L, L).
preorder_dl(t(K, L, R), [K|LT], LE) :- preorder_dl(L, LT, L1),
                                   preorder_dl(R, L1, LE).

postorder_dl(nil, L, L).
postorder_dl(t(K, L, R), LS, LE) :- postorder_dl(L, LS, LT),
                                    postorder_dl(R, LT, [K|LE]).

quicksort_dl([H|T], S, E) :-
    partition(H, T, Sm, Lg),
    quicksort_dl(Sm, S, [H|L]),
    quicksort_dl(Lg, L, E).
quicksort_dl([], L, L).

partition(H, [X|T], [X|Sm], Lg) :- X < H, !, partition(H, T, Sm, Lg).
partition(H, [X|T], Sm, [X|Lg]) :- partition(H, T, Sm, Lg).
partition(_, [], [], []).

% side effects
:-dynamic memo_fib/2.

fib(N, F) :- memo_fib(N, F), !.
fib(N, F) :- N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2,
    assertz(memo_fib(N, F)).

fib(0, 1).
fib(1, 1).

print_all :- memo_fib(N, F),
    write(N),
    write('-'),
    write(F),
    nl,
    fail.
print_all.

perm(L, [H|R]) :- append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

% generate all of the permutations, and store them in knowledge base.
all_perm(L, _) :- perm(L, L1),
                  assertz(p(L1)),
                  fail.
% if there are no more permutation, collect them
all_perm(_, R) :- collect_perms(R).

% collect by deleting from the knowledge base.
collect_perms([L1|R]) :- retract(p(L1)),
                         !,
                         collect_perms(R).
collect_perms([]).

% Q10-4

% 1.
% transform an incomplete list into a difference list, and vice-versa.

% if we are at the end of the incomplete list, make an empty diff list.
il_to_dl(L, LS, LS) :- var(L), !.
% the copy the elements of the list
il_to_dl([H|T], [H|NT], LE) :- il_to_dl(T, NT, LE).

% just keep the start part, the difference list is already terminated by
% a variable.
% alternatively copy the elements, one by one.
dl_to_il(L, _, L).

% 2.
% transform complete list into difference list, and vice-versa.

% if we are at the end of the complete list, make an empty diff list.
cl_to_dl([], LS, LS).
% copy the rest of the list.
cl_to_dl([H|T], [H|NT], LE) :- cl_to_dl(T, NT, LE).

% at the end of the list.
dl_to_cl(LS, LS, []) :- var(LS), !.
% copy elements.
dl_to_cl([H|T], LE, [H|NT]) :- dl_to_cl(T, LE, NT).
% alternatively just replace LE, by [].

% 3.
% generate a list with all possible decompositions, without findall.

all_decompositions(L, _) :-
        append(F, S, L),
        assertz(d([F, S])),
        fail.
all_decompositions(_, List) :-
        collect_decomps(List).

collect_decomps([R|List]) :-
        retract(d(R)),
        !,
        collect_decomps(List).
collect_decomps([]).

% P10-5

% 1.
% flatten a deep list using difference lists instead of append.

% if we are at the end of the deep list, make an empty diff list.
flatten_dl([], L, L).
% if we have an atomic head, just add it to the list.
flatten_dl([H|T], [H|LS], LE) :- atomic(H), !, flatten_dl(T, LS, LE).
% if the head is a deep list, flatten it, and add to the end of that list
% the result of flattening the tail.
flatten_dl([H|T], LS, LE) :- flatten_dl(H, LS, NE), flatten_dl(T, NE, LE).

% 2.
% collects all even keys in a binary tree, using difference lists.

% end of tree, make empty diff list.
collect_even_dl(nil, LL, LL).
% Key is even, include it, start search from here.
collect_even_dl(t(K, L, R), LS, LE) :-
    0 is mod(K, 2), !,
    collect_even_dl(L, LS, [K|LT]),
    collect_even_dl(R, LT, LE).
% key not even, start search in subtrees.
collect_even_dl(t(_, L, R), LS, LE) :-
    collect_even_dl(L, LS, LT),
    collect_even_dl(R, LT, LE).

% 3.
% collect from an incomplete tree all the keys between K1 and K2.
% in other words in the interval: (K1, K2)
% use differnce lists in the solution.
% I suppose that K1 =< K2

% if we are at the end of the tree, generate an empty diff list.
search_and_collect_it(T, _, _, LL, LL) :- var(T), !.
% if we are in the interval, search in both directions, and
% add the current key to the list of solutions.
search_and_collect_it(t(K, L, R), K1, K2, LS, LE) :-
    K > K1,
    K < K2,
    !,
    search_and_collect_it(L, K1, K2, LS, [K|LT]),
    search_and_collect_it(R, K1, K2, LT, LE).
% if we have a key which is smaller or equal than the lower bound, go
% to the right, where bigger numbers can be found.
search_and_collect_it(t(K, _, R), K1, K2, LS, LE) :-
    K =< K1, 
    !,
    search_and_collect_it(R, K1, K2, LS, LE).
% in the other case search in the left subtree, where smaller numbers can be 
% found.
search_and_collect_it(t(_, L, _), K1, K2, LS, LE) :-
    search_and_collect_it(L, K1, K2, LS, LE).
