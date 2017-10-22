directlyIn(katarina, olga).
directlyIn(olga, natasha).
directlyIn(natasha, irina).
% directlyIn(irina, katarina). -- bad idea!

in(X, Y) :- directlyIn(X, Y).
in(X, Y) :- directlyIn(X, Z),
                in(Z, Y).
