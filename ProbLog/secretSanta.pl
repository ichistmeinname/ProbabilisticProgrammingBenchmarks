:- use_module(library(lists)).

P::giveP(_,P).

%uniform([X|XS],Y) :- length([X|XS],Len), P is 1/Len, uniform([X|XS],1,P,Y).
%uniform([X|Xs],W,P,X) :- P1 is P/W, giveP(X,P1).
%uniform([X|Xs],W,P,Y) :-
%  P1 is P/W, not giveP(X,P1), W1 is W-P, uniform(Xs,W1,P,Y).

% reuse select_uniform as it's not trivial to define
% uniform/2 with the expected behaviour
uniform([X|XS],Y) :- select_uniform(42,[X|XS],Y,ZS).

pPicks(P,Hat,V) :- uniform(Hat,V), V \== P.

pickRound(Hat,Arrs) :- pickRound(Hat,Hat,Arrs).
pickRound([],_,[]).
pickRound([P|Ps], Hat, [(P,V)|Arrs]) :- pPicks(P,Hat,V), delete(Hat,V,HatNew), pickRound(Ps,HatNew, Arrs).

ppPicks(P,Hat,just(V)) :- uniform(Hat,V), V \== P.
ppPicks(P,Hat,nothing) :- uniform(Hat,V), V = P.

ppickRound(Hat,Arrs) :- ppickRound(Hat,Hat,Arrs).
ppickRound([],_,[]).
ppickRound([P|Ps], Hat, [(P,V)|Arrs]) :- ppPicks(P,Hat,just(V)), delete(Hat,V,HatNew), ppickRound(Ps,HatNew, Arrs).
ppickRound([P|Ps], Hat, [failedGame]) :- ppPicks(P,Hat,nothing).

is_pair((X,Y)).

allValid([]).
allValid([X|Xs]) :- is_pair(X), allValid(Xs).

anyFailed(Xs) :- member(failedGame,Xs).

hat(2,[2,1]).
hat(N,[N|Xs]) :- N > 1, M is N-1, hat(M,Xs).

% clever pick: a person cannot pick herself
% invalid games are just `false`
pickRound2(Hat,Arrs) :- pickRound2(Hat,Hat,Arrs).
pickRound2([],_,[]).
pickRound2([P|Ps], Hat, [(P,V)|Arrs]) :- delete(Hat,P,HatTemp), pPicks(P,HatTemp,V), delete(Hat,V,HatNew), pickRound2(Ps,HatNew, Arrs).


% clever pick: a person cannot pick herself
% invalid games are tracked as well
ppickRound2(Hat,Arrs) :- ppickRound2(Hat,Hat,Arrs).
ppickRound2([],_,[]).
ppickRound2([P],[P],[failedGame]).
ppickRound2([P|Ps], Hat, [(P,V)|Arrs]) :- delete(Hat,P,HatTemp), pPicks(P,HatTemp,V), delete(Hat,V,HatNew), ppickRound2(Ps,HatNew, Arrs).

isValid([],true).
isValid([X|Xs],false) :- X == failedGame.
isValid([X|Xs],Bool) :- X \= failedGame, isValid(Xs,Bool).

santa1(N,Arrs) :- hat(N,Xs), pickRound(Xs,Arrs).
santa1(N) :- santa1(N,_).

santa2(N,Arrs) :- hat(N,Xs), ppickRound(Xs,Arrs).
santa2(N) :- santa2(N,_).
santa21(N,Arrs) :- santa2(N,Arrs), allValid(Arrs).
santa21(N) :- santa21(N,_).
santa22(N,Arrs) :- santa2(N,Arrs), anyFailed(Arrs).
santa22(N) :- santa22(N,_).

santa3(N,Arrs) :- hat(N,Xs), pickRound2(Xs,Arrs).
santa3(N) :- hat(N,Xs), pickRound2(Xs,_).

santa4(N,Arrs) :- hat(N,Xs), ppickRound2(Xs,Arrs).
santa41(N,Arrs) :- hat(N,Xs), ppickRound2(Xs,Arrs), allValid(Arrs).
santa42(N,Arrs) :- hat(N,Xs), ppickRound2(Xs,Arrs), anyFailed(Arrs).
santa4(N) :- santa4(N,_).
santa41(N) :- santa41(N,_).
santa42(N) :- santa42(N,_).

santa51(N,Bool) :- hat(N,Xs), ppickRound2(Xs,Arrs), isValid(Arrs,Bool).
santa52(N,Bool) :- hat(N,Xs), ppickRound(Xs,Arrs), isValid(Arrs,Bool).

query(santa51(3,X)).
query(santa52(3,X)).
%query(santa41(10)).
%query(santa42(5)).
%query(santa21(10)).
%query(santa22(5)).
% query(santa41(3)).
% query(santa42(3)).
% query(santa3(3)).
% query(santa2(3)).
% query(santa21(3)).
% query(santa22(3)).
% query(santa1(3)).
