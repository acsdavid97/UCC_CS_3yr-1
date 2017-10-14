loves(vincent, mia).
loves(marsellus, mia).
loves(pumpkin, honey_bunny).
loves(honey_bunny, pumkin).

jealous(X, Y):- loves(X, Z), loves(Y, Z).
