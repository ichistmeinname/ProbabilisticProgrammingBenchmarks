from problog.program import PrologString
from problog.core import ProbLog
from problog import get_evaluatable
from problog.logic import Term, Constant

import sys

model = """
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

query(allRepSix(N)) :- len(N).
"""

if (len(sys.argv) == 2) :
    n = sys.argv[1]
    len_n = "len(%s)." % n
    knowledge = get_evaluatable().create_from(model + len_n).evaluate()

print knowledge
