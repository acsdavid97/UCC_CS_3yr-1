% calculates the depth of a list.
%
% an empty list has depth of 1.
depth([], 1) :- !.
% an atomic element has a depth of 0.
depth(A, 0) :- atomic(A), !.
% if the element in atomic, skip it.
depth([H|T], R) :- atomic(H), !, depth(T, R).
% result is the max of: depth of the Head + 1, or depth of Tail.
depth([H|T], R) :- depth(H, R1), depth(T, R2), R3 is R1 + 1, max(R3, R2, R).

% calculates max of two numbers.
max(N1, N2, R) :- N1 > N2, !, R = N1.
max(_, N2, R) :- R = N2.

% flattens a list: each element of a deep list will be in a single list, having
% depth of 1.
%
% empty list is the empty list flattened.
flatten([], []).
% if head is atomic, include it in the result.
flatten([H|T], [H|R]) :- atomic(H), !, flatten(T, R).
% if head is nested, flatten it, append it to the flattened tail.
flatten([H|T], R) :- flatten(H, R1), flatten(T, R2), append(R1, R2, R).

% return all atomic elements, which are at the head of a shallow list.
%
% empty list -> empty list
heads3([], [], _).
% if head is atomic and flag 1, result is head + heads of tail.
heads3([H|T], [H|R], 1) :- atomic(H), !, heads3(T, R, 0).
% if head is atomic and flag 0, result is just heads of tail.
heads3([H|T], R, 0) :- atomic(H), !, heads3(T, R, 0).
% if head is not atomic, results is heads of head, and heads of tail.
heads3([H|T], R, _) :- heads3(H, R1, 1), heads3(T, R2, 0), append(R1, R2, R).
% pretty call, to make the flag disappear. 
heads_pretty(L, R) :- heads3(L, R, 1).

% if the element is the same as the head of the list, it is a member.
member1(H, [H|_]).
% if element is member of the head it is a member of the list.
member1(X, [H|_]) :- member1(X, H).
% if element is member of the tail it is a member of the list.
member1(X, [_|T]) :- member1(X, T).

% Q7.3

% 1.
%  number of atomic elements in a deep list.

% empty list has 0 atomic elements.
nr_atomic([], 0).
% if head is atomic, add one to the total.
nr_atomic([H|T], R) :- atomic(H), !, nr_atomic(T, R1), R is R1 + 1.
% result is atomic elements in the head + atomic elements in the tail.
nr_atomic([H|T], R) :- nr_atomic(H, R1), nr_atomic(T, R2), R is R1 + R2.

% 2.
% sum of atomic elements.
% empty list has the sum equal to zero by convention.
sum_atomic([], 0).
% if head is atomic, add it to the total.
sum_atomic([H|T], R) :- atomic(H), !, sum_atomic(T, R1), R is R1 + H.
% result is sum of atomic elements in the head + atomic elements in the tail.
sum_atomic([H|T], R) :- sum_atomic(H, R1), sum_atomic(T, R2), R is R1 + R2.

% 3.
% deterministic member.

% if the element is the same as the head of the list, it is a member.
% exclamation mark to make it deterministic.
member_det(H, [H|_]) :- !.
% if element is member of the head it is a member of the list.
member_det(X, [H|_]) :- member_det(X, H).
% if element is member of the tail it is a member of the list.
member_det(X, [_|T]) :- member_det(X, T).

% P7.4

% 1.
% return elements from a deep lists, which are at the end of the 
% shallow list.

% base case
ends_acc([], Acc, Acc) :- !.
% if head is atomic, put it in the results.
ends_acc([H], Acc, R) :- atomic(H), !, ends_acc([], [H|Acc], R).
% if head is atomic, but not the last element, skip it.
ends_acc([H|T], Acc, R) :- atomic(H), !, ends_acc(T, Acc, R).
% the result is the ends in the head and the ends in the tail combined.
ends_acc([H|T], _, R) :- ends_acc(H, [], R1), ends_acc(T, [] , R2), 
                        append(R1, R2, R).
% pretty call to get rid of the accumulator.
ends_pretty(L, R) :- ends_acc(L, [], R).


% 2.
% replace an element/list/deep list with another expression in a deep list.
% L  - initial deep list
% TR - to be replaced
% E  - expression, which will appear in place of TR, after operation
% R  - resulting deep list.
% S  - success - 1, if replaced TR, 0 if not.

% base case, no replace.
replace_deep([], _, _, [], 0).

% do the replace if the head is the same as TR.
replace_deep([TR|T], TR, E, [E|T], 1) :- !.
% if head is not TR and head is atomic, skip it.
replace_deep([H|T], TR, E, [H|R], S) :- atomic(H), !, 
                                        replace_deep(T, TR, E, R, S).

% if head is not atomic replace in head, or replace in tail (but not both).
replace_deep([H|T], TR, E, [RH|RT], S) :- replace_deep(H, TR, E, RH, S1), 
                                          S1 = 1, !, RT = T, S = 1 ;
                                          S1 = 0, RH = H, replace_deep(T, TR, E, RT, S).

replace_deep_pretty(L, TR, E, R) :- replace_deep(L, TR, E, R, _).

% 3.
% sort deep lists, based on their depth
% if their depth is the same, then order them lexicographically.

sort_deep([H|T], R) :- partition(H, T, Sm, Lg), sort_deep(Sm, SmS),
            sort_deep(Lg, LgS), append(SmS, [H|LgS], R).
sort_deep([], []).

partition(H, [X|T], [X|Sm], Lg) :- smaller(X, H), !, partition(H, T, Sm, Lg).
partition(H, [X|T], Sm, [X|Lg]) :- partition(H, T, Sm, Lg).
partition(_, [], [], []).

% if both of them are atomic, consider their value
smaller(A, B) :- atomic(A), atomic(B), !, A < B.
% if both of them are deep lists, consider their depth.
smaller(A, B) :- (depth(A, DA), depth(B, DB)), DA < DB ;
% if the depth of the lists are the same, compare them lexicographically.
                 smaller_lexi(A, B).

% if if head of A is smaller then head of B, then A is smaller
smaller_lexi([HA|_], [HB|_]) :- smaller(HA, HB), !.
% if head of A is the same as head of B, then compare the tails.
smaller_lexi([HA|TA], [HB|TB]) :- HA = HB, smaller_lexi(TA, TB).

