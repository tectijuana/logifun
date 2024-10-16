male(harry).
female(liz).

parent(phil, chas).
parent(liz, chas).
parent(chas, harry).
parent(chas, wills).

grandmother(GM, C):-  
    mother(GM, P),  
    parent(P, C).

mother(M,C):-  
    female(M),  
    parent(M, C).
