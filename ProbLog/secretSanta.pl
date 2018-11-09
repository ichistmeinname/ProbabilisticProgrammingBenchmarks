:- use_module(library(lists)).

P::giveP(X,P).

uniform([X|XS],Y) :- length([X|XS],Len), P is 1/Len, uniform([X|XS],P,Y).
uniform([X|Xs],P,X) :- giveP(X,P).
uniform([_|Xs],P,X) :- uniform(Xs,P,X).

% query(uniform([1,2,3],X)).
% query(select_uniform(42,[1,2,3],X,_)).

pPicks(P,Hat,V) :- uniform(Hat,V), V \= P.

pickRound(Hat,Arrs) :- pickRound(Hat,Hat,Arrs).
pickRound([],_,[]).
pickRound([P|Ps], Hat, [(P,V)|Arrs]) :- pPicks(P,Hat,V), delete(Hat,V,HatNew), pickRound(Ps,HatNew, Arrs).

pickRound2(Hat) :- pickRound2(Hat,Hat).
pickRound2([],_).
pickRound2([P|Ps], Hat) :- pPicks(P,Hat,V), delete(Hat,V,HatNew), pickRound2(Ps,HatNew).

% query(pPicks(1,[1,2,3],V)).
% query(pickRound([1,2,3],Arrs)).
% query(pickRound2([1,2,3])).

queryFind(2) :- findall(X,pickRound([1,2,3],X),L), length(L,N), N > 0.

query(queryFind(L)).

ppPicks(P,Hat,just(V)) :- uniform(Hat,V), V \= P.
ppPicks(P,Hat,nothing) :- uniform(Hat,V), V = P.

ppickRound(Hat,Arrs) :- ppickRound(Hat,Hat,Arrs).
ppickRound([],_,[]).
ppickRound([P|Ps], Hat, [(P,V)|Arrs]) :- ppPicks(P,Hat,just(V)), delete(Hat,V,HatNew), ppickRound(Ps,HatNew, Arrs).
ppickRound([P|Ps], Hat, [failedGame]) :- ppPicks(P,Hat,nothing).

is_pair((X,Y)).

allValid([]).
allValid([X|Xs]) :- is_pair(X), allValid(Xs).  

anyFailed([failedGame]).
anyFailed([failedGame|[X|Xs]]).
anyFailed([X|Xs]) :- X \= failedGame, anyFailed(Xs).
  
hat(2,[2,1]).
hat(N,[N|Xs]) :- N > 1, M is N-1, hat(M,Xs).

testQuery(N,Arrs) :- hat(N,Xs), ppickRound(Xs,Arrs).
testQuery2(N,tt) :- hat(N,Xs), ppickRound(Xs,Arrs).
santa1(N,tt) :- hat(N,Xs), ppickRound(Xs,Arrs), allValid(Arrs).
santa2(N,tt) :- hat(N,Xs), ppickRound(Xs,Arrs), anyFailed(Arrs).

%query(allValid([(1,2),(2,3),(4,2),failedGame])).
%query(ppickRound([1,2,3],Arrs)) :- allValid(Arrs).
%query(santa1(3,_)).
query(santa2(3,_)).
%query(testQuery2(3,_)).

0.5::coin(tt).
0.5::coint(ff).
queryCoin(success) :- coin(tt).
%query(queryCoin(X)).
