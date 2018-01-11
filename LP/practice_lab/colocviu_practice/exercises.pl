% 1.
% Count the number of lists in a deep list.
count_lists([], 0).
count_lists([H|T], R) :- atomic(H), !, count_lists(T, R).
count_lists([H|T], R) :- count_lists(H, RH), count_lists(T, RT), R is RH + RT + 1.

% 2.
% double the odd numbers and square the even.
numbers([], []).
numbers([H|T], [H1|NT]) :- 1 is mod(H, 2), !, H1 is H * 2, numbers(T, NT).
numbers([H|T], [H1|NT]) :- H1 is H * H, numbers(T, NT).

% 3.
% convert a number to binary 
to_binary_rec(0, [0]).
to_binary_rec(1, [1]).
to_binary_rec(N, [1|R]) :- N > 1, 1 is mod(N, 2), !, 
    N1 is div(N, 2),N1 is div(N, 2), to_binary_rec(N1, R).
to_binary_rec(N, [0|R]) :- N > 1, 0 is mod(N, 2), 
    N1 is div(N, 2), N1 is div(N, 2), to_binary_rec(N1, R).

to_binary(N, R) :- to_binary_rec(N, RR), reverse(RR, R).

% 5.
% delete occurences of x on even positions (position numbering starts from 1).
delete_pos_even_rec([], _, _,  []).
delete_pos_even_rec([X|T], X, P, NT) :- 0 is mod(P, 2), !,
    P1 is P + 1,
    delete_pos_even_rec(T, X, P1, NT).
delete_pos_even_rec([H|T], X, P, [H|NT]) :-
    P1 is P + 1,
    delete_pos_even_rec(T, X, P1, NT).

delete_pos_even(L, X, R) :- delete_pos_even_rec(L, X, 1, R).

% 7.
% reverse a natural number
reverse_num(0, Acc, Acc) :- !.
reverse_num(N, Acc, R) :- NN is div(N, 10), RN is mod(N, 10), 
    Acc1 is 10 * Acc + RN, 
    reverse_num(NN, Acc1, R).

% 8.
% delete each k-th element from the end of the list.
delete_kth_end(L, K, R) :-
    reverse(L, RL),
    delete_kth(RL, K, RR),
    reverse(RR, R).

delete_kth(L, K, R) :-
    delete_kth_rec(L, K, 1, R).

delete_kth_rec([], _, _, []).
delete_kth_rec([_|T], K, K, NT) :-
    delete_kth_rec(T, K, 1, NT), !.
delete_kth_rec([H|T], K, P, [H|NT]) :-
    P1 is P + 1,
    delete_kth_rec(T, K, P1, NT).

% 10.
% binary incomplete tree, collect odd nodes with 1 child in an il.

tree(t(26,t(14,t(2,_,_),t(15,_,_)),t(50,t(35,t(29,_,_),_),t(51,_,t(58,_,_))))).

append_il(A, B, B) :- var(A), !.
append_il([H|T], B, [H|NT]) :- append_il(T, B, NT).

collect_il(T, _) :- var(T), !.
collect_il(t(K, L, R), [K|IL]) :- 
    var(L), nonvar(R), !, collect_il(R, IL).
collect_il(t(K, L, R), [K|IL]) :- 
    var(R), nonvar(L), !, collect_il(L, IL).
collect_il(t(_, L, R), IL) :-
    collect_il(L, ILL), collect_il(R, ILR),
    append_il(ILL, ILR, IL).

% 15.
% flatten only at depth X from a deep list.
flatten_at_depth_c([], _, _, []).
flatten_at_depth_c([H|T], D, D, [H|NT]) :- atomic(H), !,
    flatten_at_depth_c(T, D, D, NT).
flatten_at_depth_c([_|T], D, D, NT) :- flatten_at_depth_c(T, D, D, NT).
flatten_at_depth_c([H|T], D, CD, NT) :-
    CD < D, atomic(H), !, flatten_at_depth_c(T, D, CD, NT).
flatten_at_depth_c([H|T], D, CD, R) :-
    CD < D, CD1 is CD + 1,
    flatten_at_depth_c(H, D, CD1, RH),
    flatten_at_depth_c(T, D, CD, RT),
    append(RH, RT, R).

% 2016 January -- lab test.

% 1.
% a)
% extract_k(L, K, X) -- extract the k-th element in X from L.

extract_k([H|_], 0, H) :- !.
extract_k([_|T], K, X) :-
    K > 0,
    K1 is K - 1,
    extract_k(T, K1, X).
% b)
% diagonal(M, D) -- extract the the element from the main diagonal of the
% matrix M
diagonal(M, D) :-
    diagonal_rec(M, 0, D).
diagonal_rec([], _, []).
diagonal_rec([H|T], K, [HD|TD]) :-
    extract_k(H, K, HD),
    K1 is K + 1,
    diagonal_rec(T, K1, TD).

% 2.
% a)
% Binary incomplete tree T. extract_even(T, L) -- collects all even elements on
% even levels.
extract_even(T, L) :- extract_even_rec(T, 1, L).

extract_even_rec(T, _, []) :- var(T), !.
extract_even_rec(t(K, L, R), CL, [K|RL]) :-
    0 is mod(CL, 2),
    0 is mod(K, 2), !,
    CL1 is CL + 1,
    extract_even_rec(L, CL1, RLL),
    extract_even_rec(R, CL1, RLR),
    append(RLL, RLR, RL).
extract_even_rec(t(_, L, R), CL, RL) :-
    CL1 is CL + 1,
    extract_even_rec(L, CL1, RLL),
    extract_even_rec(R, CL1, RLR),
    append(RLL, RLR, RL).

extract_even_dl(T, LS, LE) :- extract_even_dl_rec(T, 1, LS, LE).

extract_even_dl_rec(T, _, LE, LE) :- var(T), !.
extract_even_dl_rec(t(K, L, R), CL, [K|LS], LE) :-
    0 is mod(CL, 2),
    0 is mod(K, 2), !,
    CL1 is CL + 1,
    extract_even_dl_rec(L, CL1, LS, LT),
    extract_even_dl_rec(R, CL1, LT, LE).
extract_even_dl_rec(t(_, L, R), CL, LS, LE) :-
    CL1 is CL + 1,
    extract_even_dl_rec(L, CL1, LS, LT),
    extract_even_dl_rec(R, CL1, LT, LE).
