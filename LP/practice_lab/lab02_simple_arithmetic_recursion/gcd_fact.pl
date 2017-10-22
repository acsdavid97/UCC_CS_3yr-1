gcd(X, X, X).
gcd(X, Y, Z) :- X > Y, R is X-Y, gcd(R, Y, Z).
gcd(X, Y, Z) :- X < Y, R is Y-X, gcd(X, R, Z).

fact(0, 1).
fact(N, F) :- N > 0, N1 is N-1, fact(N1, F1), F is F1*N.

fact1(0, FF, FF).
fact1(N, FP, FF) :- N > 0, N1 is N-1, FP1 is FP*N, fact1(N1, FP1, FF).

do(X, I,  R) :- R is X+I.

for(In, In, 0).
for(In, Out, I):-
    I > 0,
    NewI is I-1,
    do(In, I,  Intermediate),
    for(Intermediate, Out, NewI).

lcm(X, Y, Z) :- gcd(X, Y, G), Z is X*Y/G.

fib(0, 1).
fib(1, 1).
fib(N, R) :- 
    N > 1,
    N1 is N-1, fib(N1, F1),
    N2 is N-2, fib(N2, F2),
    R is F1 + F2.

printIt(Low, Next) :-
    Next is Low+1,
    print(Low).

repeatUntil(Low, Low).
repeatUntil(Low, High) :-
    printIt(Low, Next),
    Low < High,
    repeatUntil(Next, High).

while(Low, Low).
while(Low, High) :-
    Low < High,
    printIt(Low, Next),
    while(Next, High).

triangle(A, B, C) :- 
    AB is A+B, AB > C,
    AC is A+C, AC > B,
    BC is B+C, BC > A.

solve_eq2(A, B, C, X) :-
    D is sqrt(B*B -4*A*C),
    X1 is (-B - D)/(2*A),
    X2 is (-B + D)/(2*A),
    (X = X1; X = X2).
