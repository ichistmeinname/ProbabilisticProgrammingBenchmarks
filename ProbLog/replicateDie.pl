% In ProbLog everything is tabled (or memoized).
% Tabling is an advanced form of caching that is
% used to speed-up the execution of logic programs
% and that allows certain types of cyclic programs.
  
% we need to add some kind of ID in order to "disable" tabling
(1/6)::die(D,1).
(1/6)::die(D,2).
(1/6)::die(D,3).
(1/6)::die(D,4).
(1/6)::die(D,5).
(1/6)::die(D,6).

isSix(N) :- N == 6.
allSix([]).
allSix([X|XS]) :- isSix(X), allSix(XS).

% replicateDist(0,Dist,[]).
% replicateDist(N,Dist,[X|XS]) :- N \== 0, X = Dist, N1 is N-1, replicateDist(N1,Dist,XS).

replicateDie(0,[]).
replicateDie(N,[X|XS]) :- N \== 0, die(N,X), N1 is N-1, replicateDie(N1,XS).

someDie(N) :- die(42,N).
dieSix :- die(42,N), isSix(N).
allRepSix(N) :- replicateDie(N,XS), allSix(XS).

% query(dieSix).
% query(replicateDie(2,XS)).

  % The use of evidence does make much sense, as it enables a conditional probability,
  % thus, when using `evidence`, the query has probability 1.
% evidence(die(1,6)).
% evidence(die(2,6)).
% evidence(die(3,6)).
% evidence(die(4,6)).
  
query(allRepSix(5)).

% m-067:ProbLog sad$ time problog replicateDie.pl 
% allRepSix(3):	0.0046296296
% 
% real	0m0.588s
% user	0m0.506s
% sys	0m0.077s
% m-067:ProbLog sad$ time problog replicateDie.pl 
% allRepSix(4):	0.00077160494 
% 
% real	0m0.264s
% user	0m0.145s
% sys	0m0.097s
% m-067:ProbLog sad$ time problog replicateDie.pl 
% allRepSix(4):	1         
% 
% real	0m9.562s
% user	0m9.413s
% sys	0m0.108s
% m-067:ProbLog sad$ time problog replicateDie.pl 
% allRepSix(5):	0.00012860082
% 
% real	5m40.099s
% user	5m33.654s
% sys	0m1.566s
