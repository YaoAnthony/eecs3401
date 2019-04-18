/** ---------------------------------------------------------

EECS 3401 Fall 2018 Assignment 2

Family name: Stefanovich

Given name: Vladimir

Student number: 214962740



---------------------------------------------------------- */

/* load the three search algorithms */
:- ensure_loaded('astar.pl').
:- ensure_loaded('astarCC.pl').
:- ensure_loaded('idastar.pl').

/* ------------------------------------------------------- */

/* successors( +State, -Neighbors)

   Neighbors is a list of elements (Cost, NewState) where
   NewState is a state reachable from State by one action and
   Cost is the cost for that corresponding action (=1 in our
   case)
*/   
% successors( State, Succs ) :-
% 	...

successors(S,Succs) :- left(S,Sl), right(S,Sr), up(S,Su), down(S,Sd), Succs=[[1,Sl],[1,Sr],[1,Su],[1,Sd]].

left(S,Sl) :- length(S, J),N is J+1,pos(S,Col,Row,J,N),Col<J, newL(J,1,Row,Col,S,Sl), !; Sl=[].
right(S,Sr) :- length(S, J),N is J+1,pos(S,Col,Row,J,N),Col>1, newR(J,1,Row,Col,S,Sr), !; Sr=[].
up(S,Su) :- length(S, J),N is J+1,pos(S,Col,Row,J,N),Row<J,Y is Row+1, row(S,Y,R), elem(R,Col,Int), newU(J,1,Row,Y,Int,S,Su), !; Su=[].
down(S,Sd) :- length(S, J),N is J+1,pos(S,Col,Row,J,N),Row>1,Y is Row-1, row(S,Y,R), elem(R,Col,Int), newD(J,1,Row,Y,Int,S,Sd), !; Sd=[].

newL(N,Row,Row,Col,[H|T],[NH|L]) :- N1 is N-1, Count1 is Row+1, Q is Col+1, elem(H,Q,Int), swap(0,Int,H,NH), newL(N1,Count1,Row,Col,T,L), !.
newL(N,Count,Row,Col,[H|T],[H|L]) :- N1 is N-1, Count1 is Count+1, newL(N1,Count1,Row,Col,T,L), !.
newL(0,_,_,_,[H|_],[H|_]).
newL(_,_,_,_,[],[]).

newR(N,Row,Row,Col,[H|T],[NH|L]) :- N1 is N-1, Count1 is Row+1, Q is Col-1, elem(H,Q,Int), swap(Int,0,H,NH), newR(N1,Count1,Row,Col,T,L), !.
newR(N,Count,Row,Col,[H|T],[H|L]) :- N1 is N-1, Count1 is Count+1, newR(N1,Count1,Row,Col,T,L), !.
newR(0,_,_,_,[H|_],[H|_]).
newR(_,_,_,_,[],[]).

newU(N,Row,Row,NumRow,Int,[H|T],[NH|L]) :- N1 is N-1, Count1 is Row+1, replace(H,0,Int,NH), newU(N1,Count1,Row,NumRow,Int,T,L).
newU(N,NumRow,Row,NumRow,Int,[H|T],[NH|L]) :- N1 is N-1, Count1 is Row+1, replace(H,Int,0,NH), newU(N1,Count1,Row,NumRow,Int,T,L).
newU(N,Count,Row,NumRow,Int,[H|T],[H|L]) :- N1 is N-1, Count1 is Count+1, newU(N1,Count1,Row,NumRow,Int,T,L).
newU(0,_,_,_,_,[H|_],[H|_]).
newU(_,_,_,_,_,[],[]).


newD(N,NumRow,Row,NumRow,Int,[H|T],[NH|L]) :- N1 is N-1, Count1 is NumRow+1, replace(H,Int,0,NH), newD(N1,Count1,Row,NumRow,Int,T,L).
newD(N,Row,Row,NumRow,Int,[H|T],[NH|L]) :- N1 is N-1, Count1 is Row+1, replace(H,0,Int,NH), newD(N1,Count1,Row,NumRow,Int,T,L).
newD(N,Count,Row,NumRow,Int,[H|T],[H|L]) :- N1 is N-1, Count1 is Count+1, newD(N1,Count1,Row,NumRow,Int,T,L).
newD(0,_,_,_,_,[H|_],[H|_]).
newD(_,_,_,_,_,[],[]).

%HELPER FUNCTIONS 

pos([H|R],Col,Row,J,N) :-
    nth1(Col, H, 0), Row is N-J; J>0,J1 is J-1, pos(R,Col,Row,J1,N).

swap(E1,E2,[E1,E2|R],[E2,E1|R]).
swap(E1,E2,[E2,E1|R],[E1,E2|R]).
swap(E1,E2,[A|RI],[A|RO]) :- swap(E1,E2,RI,RO), !.

elem([H|R],Col,Int) :- Col>1, N is Col-1, elem(R,N,Int), !; Int is H, !.

row([_|R],X,L) :- X>1, X1 is X-1, row(R,X1,L), !.
row([H|_],X,L) :- X=1, append([],H,L).

replace([X|T],X,Y,[Y|L]) :- replace(T,X,Y,L), !.
replace([H|T],X,Y,[H|L]) :- replace(T,X,Y,L).
replace([],_,_,[]).


/* ------------------------------------------------------- */


/* equality(+S1, +S2)

   holds if and only S1 and S2 describe the same state
*/      
%equality( ...

equality([I|S1],[I|S2]) :- equality(S1,S2), !.
equality(_,_).



/* ------------------------------------------------------- */

/* hfn_null( +State, -V)

   V is the null heuristic for State (=0 irrelevant of the state)
*/
hfn_null(_State, 0).



/* hfn_misplaced( +State, -V)

   V is the number of misplaced tiles in State   
*/
% hfn_misplaced( State, V) :-  ...

%flattens the nested lists and then takes the length of all matches
%this is the opposite of V so then we subtract from N*N to get V
hfn_misplaced(S,V) :- length(S,J), Max is J*J, ideal(J,J,Max,1,G), flatten(S,S1), flatten(G,G1), count(S1,G1,L), length(L,Vn), V is Max-Vn.

%stores the number of matches into a List
count([I|S],[I|G],[I|L]) :- count(S,G,L), !.
count([_|S],[_|G],L) :- count(S,G,L), !.
count([],[],[]).


/* hfn_manhattan( +State, -V)

   V is the sum over the manhattan distances between the current
   and the designated position of each tile
*/
% hfn_manhattan( State, C ) :-  ...

hfn_manhattan( State, C ) :- length(S,J), Max is J*J, ideal(J,J,Max,1,G), dist(Max,J,J,1,S,G,L), manhatSum(L,C).

pos([H|_],Int,Col,Row,J,N) :- nth1(Col,H,Int),Row is N-J+1,!. 
pos([_|R],Int,Col,Row,J,N) :- J>0,J1 is J-1,pos(R,Int,Col,Row,J1,N),!.
pos(_,_,_,_,0,_).

dist(Loop,J,N,Int,S,G,[Dist|L]) :-  Loop>1, Loop1 is Loop-1, pos(S,Int,ColS,RowS,J,N), pos(G,Int,ColG,RowG,J,N), X is abs(ColS-ColG), Y is abs(RowS-RowG), Dist is X+Y, Int1 is Int+1,dist(Loop1,J,N,Int1,S,G,L).
dist(1,_,_,_,_,_,[]).

manhatSum([H|L],X) :- manhatSum(L,X1), X is X1+H.
manhatSum([],0).


/* ------------------------------------------------------- */


/* init( +Name, -State)

   State is the initial state for problem Name
*/
% init(a,  ...
init(a, [[1,2,3],[4,8,5],[0,7,6]]) :- a=[[1,2,3],[4,8,5],[0,7,6]].

% init(b,  ...
init(b, [[8,2,6],[4,1,5],[0,7,3]]) :- b=[[8,2,6],[4,1,5],[0,7,3]].

% init(c,  ...
init(c, [[0,2,6],[4,1,5],[8,7,3]]) :- c=[[0,2,6],[4,1,5],[8,7,3]].

% init(d, ...
init(d, [[1,2,3,4],[5,6,7,8],[9,10,0,15],[13,12,11,14]]) :- 
	d=[[1,2,3,4],[5,6,7,8],[9,10,0,15],[13,12,11,14]].


/* ------------------------------------------------------- */

/* goal( +State )

   holds if and oly if State is a goal state
*/   


% goal(S) :- ...

goal(S) :- length(S,J), Max is J*J, ideal(J,J,Max,1,G), equality(S,G). 

%creates an ideal state for arbitrary size N (or goal state)
ideal(N,M,Max,Uni,[H|L]) :- N>0, N1 is N-1, incr(M,Max,Uni,H), Uni1 is Uni+M, ideal(N1,M,Max,Uni1,L).
ideal(0,_,_,_,[]).

%used to fill in the rows with increasing values.
%G = [[1,2,3],[4,5,6],[7,8,0]] for a 3x3 puzzle
incr(N,Max,Uni,[Uni|L]) :- N>1,N1 is N-1, Uni1 is Uni+1, incr(N1,Max,Uni1,L), !.
incr(1,Max,Max,[0|[]]).
incr(1,_,Uni,[Uni|[]]).


/* ------------------------------------------------------- */






/** ---------------------------------------------------------
  calling the search algorithms
  ---------------------------------------------------------- */

go(ProblemName, HFN) :-
	init(ProblemName, Init),
	astar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goCC(ProblemName, HFN) :-
	init(ProblemName, Init),
	astarCC(Init, successors, goal, HFN, Path, equality),
	writeln(Path).

goIDA(ProblemName, HFN) :-
	init(ProblemName, Init),
	idastar(Init, successors, goal, HFN, Path, equality),
	writeln(Path).


