tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))). 

inorder(t(K, L, R), List) :- inorder(L, LL), inorder(R, LR),
                                append(LL, [K|LR], List).
inorder(nil, []).

preorder(t(K, L, R), List) :- preorder(L, LL), preorder(R, LR),
                                append([K|LL], LR, List).
preorder(nil, []).

postorder(t(K, L, R), List) :- postorder(L, LL), postorder(R, LR),
                                append(LL, LR, R1), append(R1, [K], List).
postorder(nil, []).

pretty_print(nil, _).
pretty_print(t(K, L, R), D) :- D1 is D + 1, pretty_print(L, D1), print_key(K, D),
                            pretty_print(R, D1).

% prints a key at D tab from screen left margin.
print_key(K, D) :- D > 0, !, D1 is D - 1, write('\t'), print_key(K, D1).
print_key(K, _) :- write(K), nl.

search_key(Key, t(Key, _, _)) :- !.
search_key(Key, t(K, L, _)) :- Key < K, !, search_key(Key, L).
search_key(Key, t(_, _, R)) :- search_key(Key, R).

% we found the position, insert!
insert_key(Key, nil, t(Key, nil, nil)) :- write('Inserted '), write(Key), nl.
% same key found, do nothing.
insert_key(Key, t(Key, L, R), t(Key, L, R)) :- !, write('Key already in tree\n').
% we must insert in the left subtree.
insert_key(Key, t(K, L, R), t(K, NL, R)) :- Key < K, !, insert_key(Key, L, NL).
% we must insert in the right subtree.
insert_key(Key, t(K, L, R), t(K, L, NR)) :- insert_key(Key, R, NR).

delete_key(Key, nil, nil) :- write(Key), write(' not in tree\n').
% this clause covers also case for leaf(L = nil)
delete_key(Key, t(Key, L, nil), L) :- !.
delete_key(Key, t(Key, nil, R), R) :- !.

delete_key(Key, t(Key, L, R), t(Pred, NL, R)) :- !, get_pred(L, Pred, NL).

delete_key(Key, t(K, L, R), t(K, NL, R)) :- Key < K, !, delete_key(Key, L, NL).
delete_key(Key, t(K, L, R), t(K, L, NR)) :- delete_key(Key, R, NR).

get_pred(t(Pred, L, nil), Pred, L) :- !.
get_pred(t(Key, L, R), Pred, t(Key, L, NR)) :- get_pred(R, Pred, NR).

max(A, B, A) :- A > B, !.
max(_, B, B).

height(nil, 0).
height(t(_, L, R), H) :- height(L, H1), height(R, H2), max(H1, H2, H3),
                        H is H3 + 1.

% ternary trees

ter_tree1(tt(6, 
                tt(4, 
                    tt(2, nil, nil, nil), 
                    nil, 
                    tt(7, nil, nil, nil)
                  ),
                tt(5, nil, nil, nil),
                tt(9, 
                    nil, 
                    nil, 
                    tt(3, nil, nil, nil)
                  )
            )
        ).

% pretty prints a ternary tree
ter_pretty_print(tt(K, L, M, R), D) :- print_key(K, D), D1 is D + 1,
                                        ter_pretty_print(L, D1),
                                        ter_pretty_print(M, D1),
                                        ter_pretty_print(R, D1).
ter_pretty_print(nil, _).

ter_inorder(tt(K, L, M, R), List) :- ter_inorder(L, LL), ter_inorder(M, ML),
                                        ter_inorder(R, RL), append(LL, [K|ML], L1),
                                        append(L1, RL, List).
ter_inorder(nil, []).

ter_preorder(tt(K, L, M, R), List) :- ter_preorder(L, LL), ter_preorder(M, ML),
                                        ter_preorder(R, RL), append([K|LL], ML, L1),
                                        append(L1, RL, List).
ter_preorder(nil, []).

ter_postorder(tt(K, L, M, R), List) :- ter_postorder(L, LL), ter_postorder(M, ML),
                                        ter_postorder(R, RL), append(LL, ML, L1),
                                        append(L1, RL, L2), append(L2, [K], List).
ter_postorder(nil, []).

ter_height(nil, 0).
ter_height(tt(_, L, M, R), H) :- ter_height(L, HL), ter_height(M, HM), 
            ter_height(R, HR), max(HL, HM, M1), max(M1, HR, H1), H is H1 + 1.


% Q8.4

% 1. 
% print elements inorder
inorder_alt(t(K, L, R)) :- inorder_alt(L), write(K), write(','),
                           inorder_alt(R).
inorder_alt(nil).

% 2.
% delete using the hang method:
% when a node has two children, hang the left sub-tree to the right sub-tree.
delete_alt(Key, nil, nil) :- write(Key), write(' not in tree\n').
% this clause covers also case for leaf(L = nil)
delete_alt(Key, t(Key, L, nil), L) :- !.
delete_alt(Key, t(Key, nil, R), R) :- !.

% construct the new node from the right subtree.
% alter the left subtree of the right subtree to 'hang' the left subtree there.
delete_alt(Key, t(Key, L, R), t(NK, NL, NR)) :- !, 
        get_right(R, NR), 
        get_key(R, NK),
        get_left(R, H),
        hang_left(H, L, NL).

delete_alt(Key, t(K, L, R), t(K, NL, R)) :- Key < K, !, delete_alt(Key, L, NL).
delete_alt(Key, t(K, L, R), t(K, L, NR)) :- delete_alt(Key, R, NR).

get_right(t(_, _, R), R).
get_key(t(K, _, _), K).
get_left(t(_, L, _), L).

% copies the tree until it arrives to the leftmost nil element, then
% puts the H in place of the nil.
hang_left(t(K, L, R), H, t(K, HL, R)) :-
    hang_left(L, H, HL).
hang_left(nil, H, H) :- !.

% 3.
% collect in a list the keys of leaf nodes.

% base case.
collect_leafs(nil, []).
% we found a leaf, add it to a list.
collect_leafs(t(K, nil, nil), [K]) :- !.
% collect leafs in the left and the right subtree and combine 
% them together with an append.
collect_leafs(t(_, L, R), List) :- collect_leafs(L, LL), collect_leafs(R, LR),
                                    append(LL, LR, List).

% P8-5

% 1.
% diameter of a binary tree

diam(t(_, L, R), D) :- diam(L, DL), diam(R, DR), height(L, HL), height(R, HR),
                        H is HL + HR + 1, max(DL, DR, DM), max(DM, H, D).
diam(nil, 0).

% 2.
% collect nodes at the same level, in a ternary tree.

% base case.
collect_same_level(nil, _, _, []).
% if we are on the L-th level, add the key to list
collect_same_level(tt(K, _, _, _), LL, LL, [K]) :- !.
% collect all the lists from the subtree L, M, R.
collect_same_level(tt(_, L, M, R), D, LL, RA) :- D < LL,
        D1 is D + 1,
        collect_same_level(L, D1, LL, RL),
        collect_same_level(M, D1, LL, RM),
        collect_same_level(R, D1, LL, RR),
        append(RL, RM, R1),
        append(R1, RR, RA).

collect_same_level_pretty(T, L, R) :- collect_same_level(T, 0, L, R).


% an nil node is symmetric (may be not, depends on your interpretation).
symmetric(nil).
% a tree is symmetric if the left and the right side are mirrors
symmetric(t(_, L, R)) :- mirror(L, R).

% two empty nodes are mirrors of each other
mirror(nil, nil).
% a subtree is mirror of another subtree if we can mirror the 
% left and the right subtrees.
mirror(t(_, L1, R1), t(_, L2, R2)) :- 
    mirror(R1, L2), mirror(R2, L1).
