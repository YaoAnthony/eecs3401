john.
paul.
mary.
henry.
june.
helen.
adam.

father(john,paul).
father(john,mary).
father(paul,henry).
father(paul,june).
father(henry,helen).
mother(mary,adam).

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(I,Y), ancestor(X,I).
%base case checks whether X is parent of Y
%then recurse on Y with parent I and look for their ancestor

common_ancestor(X,Y,Z) :- ancestor(X,Y), ancestor(X,Z).
%looks for one ancestor, X, that is ancestor to both Y and Z

closest_common_ancestor(X,Y,Z) :- common_ancestor(X,Y,Z), parent(X,I), \+ common_ancestor(I,Y,Z).
%find X and child of I, check that I is not a common ancestor

ancestorList(X,Y,[H|T]) :- ancestor(X,H), ancestor(H,Y), ancestorList(H,Y,T).
ancestorList(X,Y,Z) :- ancestor(X,Z), ancestor(Z,Y).
%using the head of the list we check whether it is between X and Y, recursing if it is
%base case is checking whether the last element in the list is also between X and Y

%descendantTree(X,[X|[T]]) :- parent(X,C), descendantTree(C,T).
%descendantTree(X,[X]) :- \+parent(X,I).
%

descendantTree(X,[X|[T]]) :- parent(X,C), descendantTree(C,T), descendantTree(X,[T]).
descendantTree(X,[X]) :- \+parent(X,I).





%rule definition
parent(X,Y) :- mother(X,Y); father(X,Y).

%L = [john , [paul , [henry , [helen]] , [june]] , [mary , [adam]]]
