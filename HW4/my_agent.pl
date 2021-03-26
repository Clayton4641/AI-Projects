%my_agent.pl

%   this procedure requires the external definition of two procedures:
%
%     init_agent: called after new world is initialized.  should perform
%                 any needed agent initialization.
%
%     run_agent(percept,action): given the current percept, this procedure
%                 should return an appropriate action, which is then
%                 executed.
%
% This is what should be fleshed out

:- dynamic posX/1.
:- dynamic visited/3.
:- dynamic clocation/2.
:- dynamic visited/2.
:- dynamic unvisited/2.
:- dynamic direction/1. % east = 1, north = 2, west = 3, south = 4
:- dynamic hasArrow/1.
:- dynamic mstench/2.
:- dynamic mbreeze/2.
:- dynamic mglitter/2.
:- dynamic mbump/2.
:- dynamic mscream/2.
:- dynamic mwumpuslive/1.
:- dynamic arc/6.
:- dynamic yLimit/1.
:- dynamic xLimit/1.
:- dynamic moveList/1.
:- dynamic hasGold/1.
:- dynamic safeRow/1.
:- dynamic safeCol/1.

action(goforward,goforward). % all possible acitons
action(turnleft,turnleft).
action(turnright,turnright).
action(shoot,shoot).
action(grab,grab).
action(climb,climb).

assertStench(no).
assertStench(yes) :- % assert there is a stench in our location
  clocation(X,Y),
  assert(mstench(X,Y)).

assertBreeze(no).
assertBreeze(yes) :- % assert there is a breeze in our location
  clocation(X,Y),
  format('asserting breeze'), nl,
  assert(mbreeze(X,Y)).

assertGlitter(no).
assertGlitter(yes) :-
  clocation(X,Y),
  assert(mglitter(X,Y)).

assertScream(no).
assertScream(yes) :-
  retract(mwumpuslive(yes)),
  assert(mwumpuslive(no)).

assertXLimit(no).
assertXLimit(yes) :-
  clocation(X,_),
  retractall(xLimit(_)),
  assert(xLimit(X)),
  !.

assertYLimit(no).
assertYLimit(yes) :-
  clocation(_,Y),
  retractall(yLimit(_)),
  assert(yLimit(Y)),
  !.

assertBump(no).
assertBump(yes) :-
  direction(D),
  clocation(X,Y),
  (   D == 1 -> assertXLimit(yes);
  (   D == 2 -> assertYLimit(yes);
  true)),
  !.

pwampus(X,Y) :-
  X0 is X-1,
  X1 is X+1,
  Y0 is Y-1,
  Y1 is Y+1,
  ( mstench(X0,Y); % if there is known stench in any of these places X,Y might have a wampus
    mstench(X,Y1);
    mstench(X1,Y);
    mstench(X,Y0) ),
  not(visited(X,Y,_)), % if we have been in this spot before, no worry
  mwumpuslive(yes),
  !. % if the wampus is dead we dont need to worry about any of this

ppit(X,Y) :-
  X0 is X-1,
  X1 is X+1,
  Y0 is Y-1,
  Y1 is Y+1,
  ( mbreeze(X0,Y); % if there is known breeze in any of these places X,Y might have a pit
    mbreeze(X,Y1);
    mbreeze(X1,Y);
    mbreeze(X,Y0) ),
  not(visited(X,Y,_)),
  !. % if we have been here before no worry.

noBreezeOrStench(X,Y) :-
  visited(X,Y),
  not(mbreeze(X,Y)),
  not(mstench(X,Y)),
  !.

assign(X,X).

notBreeze(X,Y,C,N) :-
  (   (not(mbreeze(X,Y)), visited(X,Y)) -> M is C+1, assign(M,N); assign(C,N)).

countNotBreeze(X,Y,C) :-
  X0 is X - 1,
  X1 is X + 1,
  Y0 is Y - 1,
  Y1 is Y + 1,
  C1 is 0,
  notBreeze(X,Y0,C1,C2),
  notBreeze(X,Y1,C2,C3),
  notBreeze(X0,Y,C3,C4),
  notBreeze(X1,Y,C4,C5),
  assign(C5,C).

notWap(X,Y,C,N) :-
  safeRow(R),
  safeCol(L),
  (   (mwumpuslive(no); (not(mstench(X,Y)), visited(X,Y))) -> M is C+1, assign(M,N); assign(C,N)).

countNotWap(X,Y,C) :-
  X0 is X - 1,
  X1 is X + 1,
  Y0 is Y - 1,
  Y1 is Y + 1,
  safeRow(R),
  safeCol(L),
  (   (X==L; Y==R) -> assign(1,C);
  C1 is 0,
  notWap(X,Y0,C1,C2),
  notWap(X,Y1,C2,C3),
  notWap(X0,Y,C3,C4),
  notWap(X1,Y,C4,C5),
  assign(C5,C)).


safe(X,Y) :-
     %X0 is X - 1,
     %X1 is X + 1,
     %Y0 is Y - 1,
     %Y1 is Y + 1,
     countNotBreeze(X,Y,B),
     %write("counting breezes "), write(B), nl,
     countNotWap(X,Y,W),
     %write("counting stenches "), write(W), nl,
     (   ((B \= 0, W \= 0); visited(X,Y)) -> true; false),
   %(
    %               noBreezeOrStench(X0,Y); % no breeze or stench in a nieghboring visited block
    %               noBreezeOrStench(X,Y1);
    %               noBreezeOrStench(X1,Y);
    %               noBreezeOrStench(X,Y0); % or
    %               not(ppit(X,Y)); %no pit nor wampus
    %               not(pwampus(X,Y))
    %           ),
  !. %stop

goForward() :-
  clocation(X,Y),
  assert(visited(X,Y)),
  direction(D),
  (
  (   D == 1 -> % east
      NX is X + 1,
      retractall(clocation(_,_)),
      assert(clocation(NX,Y))
  );
  (   D == 2 -> % north
      NY is Y + 1,
      retractall(clocation(_,_)),
      assert(clocation(X,NY))
  );
  (   D == 3 -> % west
      NX is X - 1,
      retractall(clocation(_,_)),
      assert(clocation(NX,Y))
  );
  (   D == 4 -> % south
      NY is Y - 1,
      retractall(clocation(_,_)),
      assert(clocation(X,NY))
  )
  ),
  !.

rightTurn() :-
  direction(D),
  (
  (   D == 1 -> % east
      ND is 4,
      retractall(direction(_)),
      assert(direction(ND))
   );
  (   D == 2 -> % north
      ND is 1,
      retractall(direction(_)),
      assert(direction(ND))
   );
  (   D == 3 -> % west
      ND is 2,
      retractall(direction(_)),
      assert(direction(ND))
  );
  (   D == 4 -> % south
      ND is 3,
      retractall(direction(_)),
      assert(direction(ND))
  )
  ),
  !.

leftTurn() :-
direction(D),
  (
  (   D == 1 -> % east
      ND is 2,
      retractall(direction(_)),
      assert(direction(ND))
   );
  (   D == 2 -> % north
      ND is 3,
      retractall(direction(_)),
      assert(direction(ND))
   );
  (   D == 3 -> % west
      ND is 4,
      retractall(direction(_)),
      assert(direction(ND))
  );
  (   D == 4 -> % south
      ND is 1,
      retractall(direction(_)),
      assert(direction(ND))
  )
  ),
  !.

getXlim(-1,999).
getXlim(X,X).

getYlim(-1,999).
getYlim(Y,Y).

getUnvisited(X,Y,X,Y).
getUnvisited(X,Y) :-
  visited(I,J),
  Xmin is 1,
  Ymin is 1,
  xLimit(Xlim),
  yLimit(Ylim),
  getXlim(Xlim,Xmax),
  getYlim(Ylim,Ymax),
  I0 is I - 1,
  I1 is I + 1,
  J0 is J - 1,
  J1 is J + 1,
  (   ((not(visited(I0,J)), safe(I0,J), I0 >= Xmin, J >= Ymin) -> getUnvisited(I0,J,X,Y); % if elif elif ... else fail
      ((not(visited(I,J1)), safe(I,J1), I >= Xmin, J1 =< Ymax) -> getUnvisited(I,J1,X,Y);
      ((not(visited(I1,J)), safe(I1,J), I1 =< Xmax, J >= Ymin) -> getUnvisited(I1,J,X,Y);
      ((not(visited(I,J0)), safe(I,J0), I =< Xmax, J0 =< Ymax) -> getUnvisited(I,J1,X,Y);
      false%getUnvisited(1,1,X,Y) % fail if we have visited all around
      ))))),
  (   visited(X,Y) -> false; true),
  !.

updateLocation(Move) :- % bookkeeping to update location after moving
  (
  (   Move == goforward -> goForward());
  (   Move == turnright -> rightTurn());
  (   Move == turnleft -> leftTurn());
    1 == 1 % force true
  ),
  !.

popMoves([_|T],T).
popMoves() :-
  moveList(Moves),
  popMoves(Moves,T),
  retractall(moveList(_)),
  assert(moveList(T)),
  !.

makeinternalarcs(X,Y) :-
  atom_concat(X,Y,Cords),
  atom_concat(Cords,'E',East),
  atom_concat(Cords,'N',North),
  atom_concat(Cords,'W',West),
  atom_concat(Cords,'S',South),
  (   not(arc(East,North,1,turnleft,X,Y)) -> % if the arcs don't exist make them
  assert(arc(East,North,1,turnleft,X,Y)),
  assert(arc(North,West,1,turnleft,X,Y)),
  assert(arc(West,South,1,turnleft,X,Y)),
  assert(arc(South,East,1,turnleft,X,Y)),
  assert(arc(East,South,1,turnright,X,Y)),
  assert(arc(South,West,1,turnright,X,Y)),
  assert(arc(West,North,1,turnright,X,Y)),
  assert(arc(North,East,1,turnright,X,Y));
  true).

makearcs() :-
  clocation(X,Y),

  makeinternalarcs(X,Y),

  atom_concat(X,Y,Cords),
  atom_concat(Cords,'E',East),
  atom_concat(Cords,'N',North),
  atom_concat(Cords,'W',West),
  atom_concat(Cords,'S',South),

  X0 is X - 1,
  X1 is X + 1,
  Y0 is Y - 1,
  Y1 is Y + 1,

  makeinternalarcs(X,Y0),
  makeinternalarcs(X,Y1),
  makeinternalarcs(X0,Y),
  makeinternalarcs(X1,Y),

  atom_concat(X,Y1,Up),
  atom_concat(X,Y0,Down),
  atom_concat(X0,Y,Left),
  atom_concat(X1,Y,Right),

  atom_concat(Up,'N',UpN),
  atom_concat(Down,'S',DownS),
  atom_concat(Left,'W',LeftW),
  atom_concat(Right,'E',RightE),

  atom_concat(Up,'S',UpS),
  atom_concat(Down,'N',DownN),
  atom_concat(Left,'E',LeftE),
  atom_concat(Right,'W',RightW),

  (   not(arc(East,RightE,1,goforward,X,Y)) -> assert(arc(East,RightE,1,goforward,X,Y)); true),
  (   not(arc(North,UpN,1,goforward,X,Y)) -> assert(arc(North,UpN,1,goforward,X,Y)); true),
  (   not(arc(South,DownS,1,goforward,X,Y)) -> assert(arc(South,DownS,1,goforward,X,Y)); true),
  (   not(arc(West,LeftW,1,goforward,X,Y)) -> assert(arc(West,LeftW,1,goforward,X,Y)); true),

  %assert(arc(East,RightE,1,goforward,X1,Y)),
  %assert(arc(North,UpN,1,goforward,X,Y1)),
  %assert(arc(South,DownS,1,goforward,X,Y0)),
  %assert(arc(West,LeftW,1,goforward,X0,Y)),


  (   not(arc(RightW,West,1,goforward,X1,Y)) -> assert(arc(RightW,West,1,goforward,X1,Y)); true),
  (   not(arc(DownN,North,1,goforward,X,Y0)) -> assert(arc(DownN,North,1,goforward,X,Y0)); true),
  (   not(arc(LeftE,East,1,goforward,X0,Y)) -> assert(arc(LeftE,East,1,goforward,X0,Y)); true),
  (   not(arc(UpS,South,1,goforward,X,Y1)) -> assert(arc(UpS,South,1,goforward,X,Y1)); true),

  %assert(arc(RightW,West,1,goforward,X,Y)),
  %assert(arc(DownN,North,1,goforward,X,Y)),
  %assert(arc(LeftE,East,1,goforward,X,Y)),
  %assert(arc(UpS,South,1,goforward,X,Y)),


  !.

% Percept = [Stench,Breeze,Glitter,Bump,Scream]
updateMap(Percept) :-
  write("===================updateMap======================="), nl,
  moveList(Moves),
  nth0(3,Percept,Bump),
  assertBump(Bump),
  write("updated bump"), nl,
  (
  member(_,Moves), Bump \= yes -> % if we made a move and not hit a wall
                             write("in update if"), nl,
      nth0(0,Moves,LastMove),
      updateLocation(LastMove),
      popMoves(),
      makearcs();
      retractall(moveList(_)),
      assert(moveList([]));
      true
  ),
  clocation(X,Y),
  assert(visited(X,Y)),
  write("current cords: "), write(X), write(" "), write(Y), nl,
  write("out update if"), nl,
  nth0(0,Percept,Stench),
  nth0(1,Percept,Breeze),
  nth0(2,Percept,Glitter),
  nth0(4,Percept,Scream),
  assertStench(Stench),
  assertBreeze(Breeze),
  assertGlitter(Glitter),
  assertScream(Scream),
  write("=========================End update map==================="), nl,
  !.

% Like an if statement; if Start and end are the same, Fpath = path,
% Fcost = Pathcost
pathFinder(Start,Start,Path,Pathcost,Path,Pathcost,Movelist,Movelist,MaxCost).

%recursivly search the graph till all items are found
pathFinder(Start,End,Path,Pathcost,Fpath,Fcost,Movelist,Fmovelist,MaxCost) :-
    %write("path finder called"), nl,
    arc(Start,Connected,Cost,Move,X,Y),
    %write(Start), write(" To "), write(Connected), nl,
    %write(Path), nl,
    visited(X,Y),
    not(member(Connected,Path)), % cycle check
    Pathcost < MaxCost,
    Newpathcost is Pathcost + Cost,
    Newpath = [Connected | Path],
    Newmoves = [Move | Movelist],
    pathFinder(Connected,End,Newpath,Newpathcost,Fpath,Fcost,Newmoves,Fmovelist,MaxCost).


dirToLet('E',1).
dirToLet('N',2).
dirToLet('W',3).
dirToLet('S',4).
dirToLet(Let) :-
  direction(D),
  dirToLet(Let,D).

getUnvisitedCords(X,Y) :-
  (   getUnvisited(I,J), hasGold(no) -> X is I, Y is J; X is 1, Y is 1).

getPathSmall(List,List).

getPathSmall(Start,End,Fmovelist,MaxCost) :-
  write("Looking for a path of length: "), write(MaxCost), nl,
  (   setof([C,NewMoves],pathFinder(Start,End,[Start],0,P,C,[],NewMoves,MaxCost),Set) ->
      nth0(0,Set,Small),
      getPathSmall(Small,Fmovelist);
      Newcost is MaxCost + 1,
      getPathSmall(Start,End,Fmovelist,Newcost)),
  !.

getNextAction(Action) :-
  write("========================getNextAction======================="), nl,
  moveList(Moves),
  write(Moves), nl,
  clocation(X,Y),
  dirToLet(Dir),
  getUnvisitedCords(I,J),
  write("starting main action block"), nl,

  (   mglitter(X,Y) ->
       write("getting gold"), nl,
      retractall(hasGold(_)),
      assert(hasGold(yes)),
      retractall(moveList(_)),
      assert(moveList([])),

      retractall(mglitter(_,_)),

      action(grab,Action);
  (   (hasArrow(yes), I == 1, J == 1, mstench(X,Y)) ->
      write("Shooting arrow"), nl,
      retractall(hasArrow(_)),
      assert(hasArrow(no)),
      (   (Dir == 'N'; Dir == 'S') -> retractall(safeCol(_)), assert(safeCol(I)); true),
      (   (Dir == 'W'; Dir == 'E') -> retractall(safeRow(_)), assert(safeRow(J)); true),
      retractall(moveList(_)),
      assert(moveList([])),
      action(shoot,Action);
  (   not(member(_,Moves)) ->
      write("Getting a new Path"), nl,
      direction(D),
      %Dir = 'N',
      % write("making new action list"), nl,
      %(   D == 1 -> Dir is 'E';
      %(   D == 2 -> Dir is 'N';
      %(   D == 3 -> Dir is 'W';
      %(   D == 4 -> Dir is 'S')))),
      write("Dir set"), nl,

      atom_concat(X,Y,XY),
      atom_concat(XY,Dir,Sarc),

  (   I == 1, J == 1,
          write("unvisited check"), nl,
          not(hasGold(yes)),
          write("gold check"), nl
          ->
          retractall(hasGold(_)),
          assert(hasGold(yes));
          true
      ),
      write("target: "), write(I), write(" "), write(J), nl,
      write("cords set"), nl,
      atom_concat(I,J,Tcords),

      assert(visited(I,J)),
      setof([A,B],visited(A,B),S),
      write("visited: "), write(S), nl,

      atom_concat(Tcords,Dir,Target),
      write(Sarc), write(" To "), write(Target), nl,
      %setof([C,NewMoves],pathFinder(Sarc,Target,[Sarc],0,P,C,[],NewMoves),Set),
      getPathSmall(Sarc,Target,Set,1),
      write("Move set: "), write(Set), nl,

      retractall(moveList(_)),

      nth0(1,Set,Path),
      %nth0(1,Fast,Path),

      write("Path extracted"), nl,
      (   hasGold(yes) -> Fpath = [climb | Path]; Fpath = Path),

      reverse(Fpath,Rpath),
      write("Path reversed"), nl,
      assert(moveList(Rpath)),
      nth0(0,Rpath,M),
      action(M,Action)
  ;
   write("getting action"), nl,
      nth0(0,Moves,Move),
      action(Move,Action)
  ))),
  write("================================get action end==================="), nl,
  !. % run pathing here


init_agent:-
  format('\n=====================================================\n'),
  format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),
  format('=====================================================\n\n'),
  retractall(hasArrow(_)),
  retractall(mwumpuslive(_)),
  retractall(unvisited(_,_)),
  retractall(unvisited(_,_)),
  retractall(posX(_)),
  retractall(clocation(_,_)),
  retractall(visited(_,_)),
  retractall(hasGold(_)),
  retractall(direction(_)),
  retractall(moveList(_)), % start facing east
  retractall(yLimit(_)),
  retractall(xLimit(_)),
  retractall(mstench(_,_)),
  retractall(mbreeze(_,_)),
  retractall(mglitter(_,_)),
  retractall(mbump(_,_)),
  retractall(mscream(_,_)),
  retractall(arc(_,_,_,_,_,_)),
  retractall(safeRow(_)),
  retractall(safeCol(_)),

  assert(safeCol(-999)),
  assert(safeRow(-999)),
  assert(hasArrow(yes)),
  assert(mwumpuslive(yes)),
  assert(posX(1)),
  assert(clocation(1,1)),
  assert(visited(1,1)),
  makearcs(),
  assert(hasGold(no)),
  assert(direction(1)),
  assert(moveList([])), % start facing east
  assert(yLimit(-1)),
  assert(xLimit(-1)).


%run_agent(Percept,Action):-
run_agent(Percept, Action):-
  format('\n=====================================================\n'),
  %format('This is run_agent(.,.):\n\t It gets called each time step.\n\tThis default one simply moves forward\n'),
  %format('You might find "display_world" useful, for your debugging.\n'),
  display_world,
  write(Percept), nl,
  updateMap(Percept),
  getNextAction(Action),
  moveList(Moves),
  write(Moves), nl,
  format('=====================================================\n\n').










