from problog.program import PrologString
from problog.core import ProbLog
from problog import get_evaluatable
from problog.logic import Term, Constant

import sys

model = """
:- use_module(library(lists)).

P::giveP(_,P).

% reuse select_uniform as it's not trivial to define
% uniform/2 with the expected behaviour
uniform([X|XS],Y) :- select_uniform(42,[X|XS],Y,ZS).

pPicks(P,Hat,V) :- uniform(Hat,V), V \== P.

ppPicks(P,Hat,just(V)) :- uniform(Hat,V), V \== P.
ppPicks(P,Hat,nothing) :- uniform(Hat,V), V = P.

ppickRound(Hat,Arrs) :- ppickRound(Hat,Hat,Arrs).
ppickRound([],_,[]).
ppickRound([P|Ps], Hat, [(P,V)|Arrs]) :- ppPicks(P,Hat,just(V)), delete(Hat,V,HatNew), ppickRound(Ps,HatNew, Arrs).
ppickRound([P|Ps], Hat, [failedGame]) :- ppPicks(P,Hat,nothing).

hat(2,[2,1]).
hat(N,[N|Xs]) :- N > 1, M is N-1, hat(M,Xs).

% clever pick: a person cannot pick herself
% invalid games are tracked as well
ppickRound2(Hat,Arrs) :- ppickRound2(Hat,Hat,Arrs).
ppickRound2([],_,[]).
ppickRound2([P],[P],[failedGame]).
ppickRound2([P|Ps], Hat, [(P,V)|Arrs]) :- delete(Hat,P,HatTemp), pPicks(P,HatTemp,V), delete(Hat,V,HatNew), ppickRound2(Ps,HatNew, Arrs).

isValid([],true).
isValid([X|Xs],false) :- X == failedGame.
isValid([X|Xs],Bool) :- X \= failedGame, isValid(Xs,Bool).

santa51(N,Bool) :- hat(N,Xs), ppickRound2(Xs,Arrs), isValid(Arrs,Bool).
santa52(N,Bool) :- hat(N,Xs), ppickRound(Xs,Arrs), isValid(Arrs,Bool).

"""

query1 = """
query(santa51(N,X)) :- len(N).
"""

query2 = """
query(santa52(N,X)) :- len(N).
"""

if (len(sys.argv) >= 2) :
    if (len(sys.argv) == 2) :
        n = sys.argv[1]
        len_n = "len(%s)." % n
        knowledge = get_evaluatable().create_from(model + len_n + query1).evaluate()
    else :
        n = sys.argv[2]
        len_n = "len(%s)." % n
        knowledge = get_evaluatable().create_from(model + len_n + query2).evaluate()

print knowledge
