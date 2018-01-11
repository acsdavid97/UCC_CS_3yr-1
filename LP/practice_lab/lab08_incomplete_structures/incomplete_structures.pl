% incomplete lists:

% examples:
list1([a, b, c|_]).
list2([1, 2, 3, 4, 5|_]).

% if the end is a variable, fail, and do not try other variants.
member_il(_, L) :- var(L), !, fail.
% found X.
member_il(X, [X|_]) :- !.
% X is not head, search in tail.
member_il(X, [_|T]) :- member_il(X, T).

% add an element if the last is a variable, and detect if the 
% element is already in the list
insert_il(X, [X|_]) :- !.
insert_il(X, [_|T]) :- insert_il(X, T).

% at the end of the list, stop.
delete_il(_, L, L) :- var(L), !.
% found element, delete it.
delete_il(X, [X|T], T) :- !.
% search for element in the tail.
delete_il(X, [H|T], [H|R]) :- delete_il(X, T, R).

% incomplete trees.

% examples
tree1(t(7, t(5, t(3, _, _), _), t(11, _, _))).
tree2(T) :- T = t(7, t(5, t(3, _, _), _), t(11, D, _)), D = t(9, _, _).

% not in the tree, fail.
search_it(_, T) :- var(T), !, fail.
% found the key, success.
search_it(X, t(X, _, _)) :- !.
% key is in left subtree, search there.
search_it(X, t(K, L, _)) :- X < K, !, search_it(X, L).
% key is in right subtree, search there.
search_it(X, t(_, _, R)) :- search_it(X, R).

insert_it(Key, t(Key, _, _)) :- !.
insert_it(Key, t(K, L, _)) :- Key < K, !, insert_it(Key, L).
insert_it(Key, t(_, _, R)) :- insert_it(Key, R).

delete_it(Key, T, T):-var(T), !, write(Key), write(' not in tree\n').
delete_it(Key, t(Key, L, R), L):-var(R), !.
delete_it(Key, t(Key, L, R), R):-var(L), !.
delete_it(Key, t(Key, L, R), t(Pred, NL, R)):-!, get_pred(L, Pred, NL).
delete_it(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_it(Key, L, NL).
delete_it(Key, t(K, L, R), t(K, L, NR)):- delete_it(Key, R, NR).
get_pred(t(Pred, L, R), Pred, L):-var(R), !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)):-get_pred(R, Pred, NR).

% Q9-3

% 1.
% predicate to append two incomplete lists (resulting incomplete as well).

% at the end of A, copy B as it is.
append_il(A, B, B) :- var(A), !.
% copy head of A to the result, and continue recursively on tail.
append_il([HA|TA], B, [HA|TR]) :- append_il(TA, B, TR).

% 2.
% reverse an incomplete list, the result should be an incomplete list as well.

% at the end of the list, the result is in Acc.
reverse_il_acc(L, Acc, Acc) :- var(L), !.
% put the head in the accumulator, and continue recursively on the tail.
reverse_il_acc([H|T], Acc, R) :- reverse_il_acc(T, [H|Acc], R).

% pretty call, and initialize the accumulator to a variable.
reverse_il(L, RL) :- reverse_il_acc(L, _, RL).

% 3.
% transform an incomplete list into a complete one.

% at the end of the list, substitute the variable with the empty list.
il_to_comp(L, []) :- var(L), !.
% copy the rest of the list.
il_to_comp([H|T], [H|R]) :- il_to_comp(T, R).

% 4.
% collect the keys of an incomplete binary tree into an incomplete list,
% using preorder traversal.

% if we are at an end node, return "empty list" (a variable).
preord_collect_it(T, _) :- var(T), !.
% do the preorder collect on the left and right subtrees.
preord_collect_it(t(K, L, R), List) :- 
    preord_collect_it(L, LList), 
    preord_collect_it(R, RList),
    append_il([K|LList], RList, List).

% 5.
% compute the height of an incomplete binary tree.

% "nil" has 0 height
height_it(T, 0) :- var(T), !.
% calculate the height: 1 + max(height(L), height(R)).
height_it(t(_, L, R), H) :- height_it(L, HL), height_it(R, HR),
    max(HL, HR, HM), H is HM + 1.

max(A, B, B) :- B > A, !.
max(A, _, A).

% 6.
% transform an incomplete tree into a complete tree.

% transform a variable into a nil.
it_to_comp(T, nil) :- var(T), !.
% copy the key, and do the transform on the left/right subtrees.
it_to_comp(t(K, L, R), t(K, NL, NR)) :- it_to_comp(L, NL), it_to_comp(R, NR).

% P9-4

% 1.
% flatten a deep incomplete list

% at the end of a list.
flat_il(L, _) :- var(L), !.
% if head is atomic add it to the results, and continue recursively
flat_il([H|T], [H|R]) :- atomic(H), !, flat_il(T, R).
% if head is not atomic start the flattening there, and in the tail too.
% combine the results of the flattens using an append.
flat_il([H|T], R) :- flat_il(H, FH), flat_il(T, FT), append_il(FH, FT, R).

% 2.
% compute the diamter of an incomplete tree

diam_it(T, 0) :- var(T), !.
% apply the formula.
diam_it(t(_, L, R), D) :- height_it(L, HL), height_it(R, HR),
                          diam_it(L, DL), diam_it(R, DR),
                          HM is HL + HR + 1,
                          max(DL, DR, DM),
                          max(DM, HM, D).

% 3.
% determine if an incomplete list is a sub-list of another incomplete list.

% checks if the first list is at the head of the second list.
start_list(L, _) :- var(L), !.
% if there is more at the and of the first list, but we arrived at the
% end of the second list: fail.
start_list(_, L) :- var(L), !, fail.
start_list([H|T], [H|R]) :- start_list(T, R).

% if we arrived at the end of the second list, the is no solution.
sub_list(_, L) :- var(L), !, fail.
% if L1 is the start of L2, we have a solution.
sub_list(L1, L2) :- start_list(L1, L2).
% try starting from the next element in the second list.
sub_list(L1, [_|T]) :- sub_list(L1, T).

% note: this solution will find the next solution, if the user wants to.
% to avoid this, change the second line of sub_lists by adding a cut.
