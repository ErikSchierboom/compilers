% British Royal family (small subset of)
male(philip).
male(william).
male(harry).
male(charles).
male(george).
female(elizabeth).
female(diana).

parent(elizabeth, charles).
parent(philip, charles).
parent(diana, william).
parent(charles, william).
parent(diana, harry).
parent(charles, harry).
parent(william, george).

% Father and mother are male/femaile parents 
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).

% TODO: grandparent and grandfather
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
grandfather(X, Y) :- grandparent(X, Y), male(X).
