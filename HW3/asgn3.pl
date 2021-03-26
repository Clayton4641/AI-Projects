%
arc(k,q,3).
arc(q,p,13).
arc(q,m,5).
arc(m,p,8).
arc(p,w,2).

% Like an if statement; if Start and end are the same, Fpath = path,
% Fcost = Pathcost
pathFinder(Start,Start,Path,Pathcost,Path,Pathcost).

%recursivly search the graph till all items are found
pathFinder(Start,End,Path,Pathcost,Fpath,Fcost) :-
    arc(Start,Connected,Cost),
    not(member(Connected,Path)), % cycle check
    Newpathcost is Pathcost + Cost,
    Newpath = [Connected | Path],
    pathFinder(Connected,End,Newpath,Newpathcost,Fpath,Fcost).

% For the way I store the length path, this gets the length of the path
minVal([H,_],M) :-
    M is H,
    true.

% pulls the first element of the given list
firstElement([H|T],Elm) :-
    Elm = H.

% prints the given path, expects it to be reversed
writePath([H|T]) :-
    firstElement(T,Path),
    reverse(Path,Cor),
    write(Cor), nl,
    true.

% the first method to call to print all the shortest paths given a list
% assumes the list is sorted
printShortest([H | Rest]) :-
    minVal(H,Min),
    writePath(H),
    printShortestRest(Rest,Min).

% second method to be called to print all the shortest paths
printShortestRest([H|T],Min) :-
    minVal(H,Curmin),
    Curmin == Min,
    writePath(H),
    printShortestRest(T,Min).

% TODO define: path(X,Y) :- ...
path(X,Y) :-
    setof([C,P],pathFinder(X,Y,[X],0,P,C),Set), % this returns all paths in set,
    % sorted by cost
    % the format is [[cost,path],...]
    %write(Set), nl,
    printShortest(Set).







