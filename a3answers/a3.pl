
/*
	FLUENTS
RobotLoc(location, situation)
BoxLoc(box, location, situation)
OnTop(box, situation)
Up(switch, situation)
LightOn(light, situation)

	NONFLUENTS
In(location, room)
Controls(switch, light)

	ENTITY TYPES
IsBox(b)
*/

/*
(RobotLoc(L,S)).	% robot is at L in situation s
(BoxLoc(b,L,S)).	% box is at L in situation s
(OnTop(b,S)).		% robot is on top of box in situation s
(Up(switch,S)).		% switch is up in situation s
(LightOn(light,S)).	% light is on in situation s

(In(L,room)).			%
(Controls(switch,light)).	%
*/

% Primitive control actions

primitive_action(Go(x,y,r)).		% moves from position x to position y in same room r
primitive_action(Push(b,x,y,r)).	% pushes box x from x to y in r
primitive_action(ClimbUp(x,b)).		% climbs from x onto b; x being floor
primitive_action(ClimbDown(b,x)).	% climbs from b down to x; x being floor
primitive_action(TurnOn(switch,b)).	% turn light switch on; must be on box
primitive_action(TurnOff(switch,b)).	% turn light switch off; must be on box

% Preconditions for Primitive Actions

poss(Go(x,y,r),S) :- In(x,r), In(y,r), RobotLoc(x,S). %The first letter of function in prolog should not be a capital letter...
poss(Push(b,x,y,r),S) :- IsBox(b), In(x,r), In(y,r), BoxLoc(b,x,S).
poss(ClimbUp(x,b),S) :-  IsBox(b).
poss(ClimbDown(x,b),S) :- IsBox(b), OnTop(b,S).
poss(TurnOn(switch,b),S) :- IsBox(b), OnTop(b,S). 
poss(TurnOff(switch,b),S) :- IsBox(b), OnTop(b,S), Up(switch,S).

In(L,room) :- L=room.


% Successor State Axioms for Primitive Fluents

RobotLoc(L,do(A,S)) :- A = Go(L,_,_); not(A = Go(L,_,_)), RobotLoc(L,S).
BoxLoc(b,L,do(A,S)) :- A = Push(b,L,_,_); not(A = Push(b,L,_,_)), BoxLoc(b,L,S).
OnTop(b,do(A,S)) :- A = ClimbDown(L,b); RobotLoc(L,S), BoxLoc(b,L,S).
Up(switch,do(A,S)) :- A = TurnOff(switch,_); Up(switch,S).
LightOn(light,do(A,S)).
IsBox(b).

% Initial Situation

locInitRobot is 3. locInitBox1 is 1. locInitBox2 is 1. locInitBox3 is 1.
locInitBox4 is 1. locInitSwitch1 is 1. locInitSwitch1 is 1. locInitSwitch2 is 2. locInitSwitch3 is 3. locInitSwitch4 is 4.
RobotLoc(locInitRobot,s0). BoxLoc(box1,locInitBox1,s0). BoxLoc(box2,locInitBox2,s0). 
BoxLoc(box3,locInitBox3,s0). BoxLoc(box4,locInitBox4 ,s0).  Up(switch1,s0). Up(switch4,s0).

% (b) Box2 to Room2

box2room2() :- do(Push(box2,Room2,Door2,Room2), do(Push(box2,Door1,Door2,Corridor), do(Push(box2,locInitBox2,Door1,Room1), do(Go(Door1,locInitBox2,Room1), do(Go(Door3,Door1,Corridor), do(Go(locInitRobot,Door3,Room3), s0)))))).
