% deterministic delete, will always delete the first item in the list (due to !).
delete(X, [X|T], T) :- !.
delete(X, [H|T], [H| R]) :- delete(X, T, R).
delete(_, [], []).

length1([], 0).
length1([_|T], Len) :- length1(T, Len1), Len is Len1 + 1.

% when reaching the empty list, unify accumulator with the free result variable
length_fwd([], Acc, Res):-Res = Acc.
% as the list is decomposed, add 1 to the accumulator; pass Res unchanged
length_fwd([_|T], Acc, Res):-Acc1 is Acc+1, length_fwd(T, Acc1, Res).

length_fwd_pretty(L, Len):-length_fwd(L, 0, Len).

reverse1([], []).
reverse1([H|T], Res):- reverse1(T, R1), append(R1, [H], Res).

reverse_fwd([], R, R).
reverse_fwd([H|T], Acc, R):- reverse_fwd(T, [H|Acc], R).
reverse_fwd_pretty(L, R):- reverse_fwd(L, [], R).

minimum([], M, M).
minimum([H|T], MP, M):- H < MP, !, minimum(T, H, M).
minimum([_|T], MP, M):- minimum(T, MP, M).

minimum_pretty([H|T], R):- minimum([H|T], H, R).

minimum_bwd([H], H).
minimum_bwd([H|T], M):-minimum_bwd(T, M), H>=M.
minimum_bwd([H|T], H):-minimum_bwd(T, M), H<M.

union1([], L, L).
union1([H|T], L2, R) :- member(H, L2), !, union1(T, L2, R).
union1([H|T], L, [H|R]) :- union1(T, L, R).

intersection1([], _, []).
intersection1([H|T], L2, [H|R]) :- member(H, L2), !, intersection1(T, L2, R).
intersection1([_|T], L2, R) :- intersection1(T, L2, R).

set_diff([], _, []).
set_diff([H|T], L2, R) :- member(H, L2), !, set_diff(T, L2, R).
set_diff([H|T], L2, [H|R]) :- set_diff(T, L2, R).

% finds and deletes the first occurence of the minimum in a list
del_min(L, R) :-
    minimum_bwd(L, M),
    delete(M, L, R).

% reverses a list starting from the K-th element.
reverse_k(L, 0, R) :- reverse1(L, R).
reverse_k([H|T], K, [H|R]) :-
    K1 is K-1,
    reverse_k(T, K1, R).

% finds the maximum in a list
maximum_bwd([H], H).
maximum_bwd([H|T], M):-maximum_bwd(T, M), H<M.
maximum_bwd([H|T], H):-maximum_bwd(T, M), H>=M.

% finds and deletes the maximum element from a list
del_max(L, R) :-
    maximum_bwd(L, M),
    delete(M, L, R).

% encodes a list using run-length encoding
rle_encode([H|T], R):-
    run_length_enc_rec([H|T], H, 0, R).

% if we have an empty list we can add the elements encountered before.
run_length_enc_rec([], H, Occ, [[H, Occ]]).
% if the same element is encountered increment Occ
run_length_enc_rec([H|T], H, Occ, R) :-
    Occ1 is Occ + 1,
    run_length_enc_rec(T, H, Occ1, R).
% if a different element is encountered at the head, add the [element, length]
% pair and start counting from 1 (since we encountered one occurance of the
% element.
run_length_enc_rec([H|T], X, Occ, [[X, Occ]|R]) :-
    X \= H,
    run_length_enc_rec(T, H, 1, R).

% rotating by zero is equivalent to the list itself.
rotate_left(L, 0, L).

% append the first element the last position
rotate_left([H|T], K, R) :-
    K > 0,
    K1 is K - 1,
    append(T, [H], R1),
    rotate_left(R1, K1, R).

% rotate right is equivalent to rotate_left by Len - K
rotate_right(L, K, R) :-
    length(L, Len),
    K1 is Len - K,
    rotate_left(L, K1, R).

% gets the k-th element
get_at_k([H|_], 0, H).
get_at_k([_|T], K, R) :-
    K > 0,
    K1 is K-1,
    get_at_k(T, K1, R).

rnd_select(L, 0, []).
rnd_select(L, K, [S|R]) :-
    length(L, Len),
    Len > 0,
    I is random(Len),
    get_at_k(L, I, S),
    delete(S, L, L1),
    K1 is K-1,
    rnd_select(L1, K1, R).






